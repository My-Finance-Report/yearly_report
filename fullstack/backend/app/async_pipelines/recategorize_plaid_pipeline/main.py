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


async def recategorize_account_pipeline(in_process_files: list[InProcessJob]) -> None:
    print("trying to apply config")
    in_process_with_config = [
        apply_upload_config_no_create(in_process) for in_process in in_process_files
    ]
    print(f"batch processing {len(in_process_with_config)}")
    await async_batch_recategorize_with_config(in_process_with_config)


async def recategorize_account_async(in_process: InProcessJob) -> None:
    def blah() -> None:
        return pipe(
            in_process,
            apply_previous_recategorizations,
            apply_existing_transactions,
            categorize_extracted_transactions,
            final=insert_recategorized_transactions,
        )

    return await asyncio.to_thread(blah)


async def recategorize_accounts_with_config_async(files: list[InProcessJob]) -> None:
    await asyncio.gather(*[recategorize_account_async(file) for file in files])


async def async_batch_recategorize_with_config(files: list[InProcessJob]) -> None:
    await recategorize_accounts_with_config_async(files)
