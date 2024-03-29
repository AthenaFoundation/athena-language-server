use crate::global_state::GlobalStateSnapshot;
use crate::{from_proto, to_proto, Result};
use anyhow::Context;
use ide::Cancellable;
use ide_db::base_db::FileRange;
use syntax::AstNode;
use tower_lsp::lsp_types::{
    GotoDefinitionParams, GotoDefinitionResponse, SemanticTokensParams, SemanticTokensResult,
    TextDocumentIdentifier,
};

#[tracing::instrument(skip(snapshot))]
pub(crate) fn semantic_tokens_full(
    snapshot: GlobalStateSnapshot,
    params: SemanticTokensParams,
) -> Result<Option<SemanticTokensResult>> {
    tracing::info!("here");
    let analysis = &snapshot.analysis;

    let Some(file_id) = snapshot
        .file_id(&params.text_document.uri)
        .ok()
    else {
        return Ok(None);
    };
    let file_id = file_id
        .with_context(|| format!("failed to get file id for uri {}", params.text_document.uri))?;
    let uri = params.text_document.uri.to_string();

    let semantic_tokens = snapshot.semantic_token_map.get(&uri).map(|v| v.clone());
    if let Some(semantic_tokens) = semantic_tokens {
        tracing::debug!("it's in the cache!");
        Ok(Some(SemanticTokensResult::Tokens(semantic_tokens)))
    } else {
        tracing::debug!("it's not in the cache!");
        let conf = snapshot.config.as_ref().clone();
        let highlight_config = conf.into();
        let index = analysis.file_line_index(file_id)?;
        let src = analysis.file_text(file_id)?;
        let highlights = analysis.highlight(highlight_config, file_id)?;
        let tokens = to_proto::semantic_tokens(&src, &index, highlights);
        snapshot.semantic_token_map.insert(uri, tokens.clone());
        Ok(Some(SemanticTokensResult::Tokens(tokens)))
    }
}

pub(crate) fn go_to_definition(
    snapshot: GlobalStateSnapshot,
    params: GotoDefinitionParams,
) -> Result<Option<GotoDefinitionResponse>> {
    let file_position = from_proto::file_position(&snapshot, params.text_document_position_params)?;

    let analysis = &snapshot.analysis;
    let Some(navs) = analysis.go_to_definition(file_position)? else {
        return Ok(None);
    };

    let locations = navs
        .info
        .into_iter()
        .map(|nav| {
            to_proto::location(
                &snapshot,
                FileRange {
                    file_id: nav.file_id,
                    range: nav.focus_or_full_range(),
                },
            )
        })
        .collect::<Cancellable<Vec<_>>>()?;

    Ok(Some(locations.into()))
}

pub(crate) fn dump_syntax_tree(
    snapshot: GlobalStateSnapshot,
    params: TextDocumentIdentifier,
) -> Result<String> {
    let Some(file_id) = snapshot
        .file_id(&params.uri)
        .ok()
    else {
        return Ok(String::new());
    };
    let file_id =
        file_id.with_context(|| format!("failed to get file id for uri {}", params.uri))?;

    let analysis = &snapshot.analysis;
    let ast = analysis.parse(file_id)?;

    Ok(format!("{:#?}", ast.tree().syntax()))
}
