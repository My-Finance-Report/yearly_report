import asyncio

from app.async_pipelines.recategorize_pipeline.main import (
    apply_previous_recategorizations,
)
from app.async_pipelines.uploaded_file_pipeline.categorizer import (
    categorize_extracted_transactions,
    insert_categorized_transactions,
    update_filejob_with_nickname,
)
from app.async_pipelines.uploaded_file_pipeline.local_types import InProcessJob
from app.async_pipelines.uploaded_file_pipeline.transaction_parser import (
    apply_upload_config,
    archive_transactions_if_necessary,
    request_llm_parse_of_transactions,
)
from app.func_utils import pipe
from app.models.worker_status import ProcessingState


from app.worker.status import log_completed, status_update_monad


def persist_config_to_job_record(in_process: InProcessJob) -> InProcessJob:
    """
    an effect that persists the config id to the job for future ease of reprocess
    """
    assert in_process.config, "must have"
    assert in_process.job, "must have"

    in_process.job.config_id = in_process.config.id
    in_process.session.add(in_process.job)

    return in_process


async def uploaded_file_pipeline(in_process_files: list[InProcessJob]) -> None:
    in_process_with_config = [
        apply_upload_config(in_process) for in_process in in_process_files
    ]
    print(f"batch processing {len(in_process_with_config)}")
    await async_batch_process_files_with_config(in_process_with_config)


async def process_file_async(in_process: InProcessJob) -> None:
    def blah() -> None:
        return pipe(
            in_process,
            lambda x: status_update_monad(
                x,
                status=ProcessingState.preparing_for_parse,
                additional_info="Building the account",
            ),
            persist_config_to_job_record,
            lambda x: status_update_monad(
                x,
                status=ProcessingState.preparing_for_parse,
                additional_info="Applying previous recategorizations",
            ),
            apply_previous_recategorizations,
            lambda x: status_update_monad(
                x,
                status=ProcessingState.preparing_for_parse,
                additional_info="Removing existing transactions if they exist",
            ),
            archive_transactions_if_necessary,
            lambda x: status_update_monad(
                x,
                status=ProcessingState.parsing_transactions,
                additional_info="Parsing transactions from file",
            ),
            request_llm_parse_of_transactions,
            lambda x: status_update_monad(
                x,
                status=ProcessingState.categorizing_transactions,
                additional_info="Categorizing batches of transactions",
            ),
            categorize_extracted_transactions,
            lambda x: status_update_monad(
                x,
                status=ProcessingState.categorizing_transactions,
                additional_info="Updating file nickname",
            ),
            update_filejob_with_nickname,
            lambda x: status_update_monad(
                x,
                status=ProcessingState.categorizing_transactions,
                additional_info="Writing transactions to the database",
            ),
            insert_categorized_transactions,
            final=lambda x: log_completed(x, additional_info="Completed upload"),
        )

    return await asyncio.to_thread(blah)


async def process_files_with_config_async(files: list[InProcessJob]) -> None:
    """Process multiple files with pre-determined configuration in parallel."""
    await asyncio.gather(*[process_file_async(file) for file in files])


async def async_batch_process_files_with_config(files: list[InProcessJob]) -> None:
    """Process multiple files with pre-determined configuration."""
    await process_files_with_config_async(files)
