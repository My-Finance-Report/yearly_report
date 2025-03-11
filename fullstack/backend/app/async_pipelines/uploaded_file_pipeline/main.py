from app.async_pipelines.uploaded_file_pipeline.categorizer import (
    categorize_extracted_transactions,
    insert_categorized_transactions,
    update_filejob_with_nickname,
)
from app.async_pipelines.uploaded_file_pipeline.local_types import InProcessFile
from app.async_pipelines.uploaded_file_pipeline.transaction_parser import (
    apply_upload_config,
    archive_transactions_if_necessary,
    request_llm_parse_of_transactions,
)
from app.func_utils import pipe
from app.async_pipelines.recategorize_pipeline.main import apply_previous_recategorizations


def persist_config_to_job_record(in_process: InProcessFile) -> InProcessFile:
    """
    an effect that persists the config id to the job for future ease of reprocess
    """
    assert in_process.config, "must have"

    in_process.job.config_id = in_process.config.id
    in_process.session.add(in_process.job)

    return in_process


def uploaded_file_pipeline(in_process: InProcessFile) -> None:
    pipe(
        in_process,
        apply_upload_config,
        persist_config_to_job_record,
        apply_previous_recategorizations,
        archive_transactions_if_necessary,
        request_llm_parse_of_transactions,
        categorize_extracted_transactions,
        update_filejob_with_nickname,
        final=insert_categorized_transactions,
    )
