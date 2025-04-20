import asyncio

from app.async_pipelines.recategorize_pipeline.main import (
    apply_existing_transactions,
    apply_previous_recategorizations,
    apply_upload_config_no_create,
    insert_recategorized_transactions,
)
from app.async_pipelines.uploaded_file_pipeline.categorizer import (
    categorize_extracted_transactions,
)
from app.async_pipelines.uploaded_file_pipeline.local_types import (
    InProcessJob,
)
from app.func_utils import pipe
from app.models import ProcessingState
from app.worker.status import log_completed, status_update_monad


async def recategorize_account_pipeline(in_process_files: list[InProcessJob]) -> None:
    in_process_with_config = [
        apply_upload_config_no_create(in_process) for in_process in in_process_files
    ]
    print(f"batch processing {len(in_process_with_config)}")
    await async_batch_recategorize_with_config(in_process_with_config)


async def recategorize_account_async(in_process: InProcessJob) -> None:
    def blah() -> None:
        return pipe(
            in_process,
            lambda x: status_update_monad(
                x,
                status=ProcessingState.preparing_for_parse,
                additional_info="Applying previous recategorizations",
            ),
            apply_previous_recategorizations,
            lambda x: status_update_monad(
                x,
                status=ProcessingState.preparing_for_parse,
                additional_info="Applying existing transactions",
            ),
            apply_existing_transactions,
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
            insert_recategorized_transactions,
            final=lambda x: log_completed(
                x, additional_info="Completed recategorization"
            ),
        )

    return await asyncio.to_thread(blah)


async def recategorize_accounts_with_config_async(files: list[InProcessJob]) -> None:
    await asyncio.gather(*[recategorize_account_async(file) for file in files])


async def async_batch_recategorize_with_config(files: list[InProcessJob]) -> None:
    await recategorize_accounts_with_config_async(files)
