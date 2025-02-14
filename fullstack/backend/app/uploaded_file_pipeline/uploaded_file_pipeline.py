import logging
from app.uploaded_file_pipeline.categorizer import CategorizedTransaction, categorize_extracted_transactions
from app.uploaded_file_pipeline.local_types import InProcessFile
from app.uploaded_file_pipeline.transaction_parser import apply_upload_config, remove_transactions_if_necessary, request_llm_parse_of_transactions
from func_utils import  safe_pipe
from typing import List




# Logging setup
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

def process_pdf_file(in_process: InProcessFile) -> List[CategorizedTransaction]:

    return safe_pipe(
        in_process,
        apply_upload_config,
        remove_transactions_if_necessary,
        request_llm_parse_of_transactions,
        final=categorize_extracted_transactions
    ) 
