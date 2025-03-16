import asyncio
import cProfile
import pstats
from collections.abc import Callable
from datetime import datetime
from functools import wraps
from typing import ParamSpec, TypeVar

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

P = ParamSpec("P")
R = TypeVar("R")


def profile_func(func: Callable[P, R]) -> Callable[P, R]:
    @wraps(func)
    def wrapper(*args: P.args, **kwargs: P.kwargs) -> R:
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


async def uploaded_file_pipeline(in_process_files: list[InProcessFile]) -> None:
    in_process_with_config = [apply_upload_config(in_process) for in_process in in_process_files]
    print(f"batch processing {len(in_process_with_config)}")
    await async_batch_process_files_with_config(in_process_with_config)


async def process_file_async(in_process: InProcessFile) -> None:
    def blah():
        return pipe(
        in_process,
        persist_config_to_job_record,
        apply_previous_recategorizations,
        archive_transactions_if_necessary,
        request_llm_parse_of_transactions,
        categorize_extracted_transactions,
        update_filejob_with_nickname,
        final=insert_categorized_transactions
        )

    return await asyncio.to_thread(blah)


async def process_files_with_config_async(files: list[InProcessFile]) -> None:
    """Process multiple files with pre-determined configuration in parallel."""
    await asyncio.gather(*[process_file_async(file) for file in files])


async def async_batch_process_files_with_config(files: list[InProcessFile]) -> None:
    """Process multiple files with pre-determined configuration."""
    await process_files_with_config_async(files)
