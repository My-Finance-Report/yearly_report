from app.uploaded_file_pipeline.categorizer import categorize_extracted_transactions, insert_categorized_transactions
from app.uploaded_file_pipeline.local_types import InProcessFile
from app.uploaded_file_pipeline.transaction_parser import apply_upload_config, remove_transactions_if_necessary, request_llm_parse_of_transactions
from func_utils import  pipe



def uploaded_file_pipeline(in_process: InProcessFile) -> None:

    pipe(
        in_process,
        apply_upload_config,
        remove_transactions_if_necessary,
        request_llm_parse_of_transactions,
        categorize_extracted_transactions,
        final=insert_categorized_transactions
    ) 


