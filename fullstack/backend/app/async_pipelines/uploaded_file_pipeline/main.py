from app.async_pipelines.recategorize_pipeline.main import (
    apply_previous_recategorizations,
)
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
import asyncio
from functools import partial
from typing import List

import cProfile
import pstats
import io
from datetime import datetime
from functools import wraps

def profile_func(func):
    @wraps(func)
    def wrapper(*args, **kwargs):
        profiler = cProfile.Profile()
        profiler.enable()
        result = func(*args, **kwargs)
        profiler.disable()

        with open(f"{func.__name__}_{datetime.now()}.profile", "w") as f:
            stats = pstats.Stats(profiler, stream=f).sort_stats("cumulative")
            stats.print_stats()

        return result
    return wrapper


def persist_config_to_job_record(in_process: InProcessFile) -> InProcessFile:
    """
    an effect that persists the config id to the job for future ease of reprocess
    """
    assert in_process.config, "must have"

    in_process.job.config_id = in_process.config.id
    in_process.session.add(in_process.job)

    return in_process


@profile_func
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



async def process_file_async(in_process: InProcessFile) -> InProcessFile:
    # Convert synchronous functions to async
    return await pipe(
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

async def process_files_async(files: List[InProcessFile]) -> List[InProcessFile]:
    # Process multiple files in parallel
    return await asyncio.gather(*[process_file_async(file) for file in files])

def async_upload_file_pipeline(in_process: InProcessFile) -> None:
    # For single file processing
    asyncio.run(process_file_async(in_process))

def async_batch_process_files(files: List[InProcessFile]) -> List[InProcessFile]:
    # For batch processing
    return asyncio.run(process_files_async(files))


