#!/usr/bin/env python3
"""
Script to run the Plaid sync scheduler
"""

import asyncio
import time

# Add the current directory to the Python path
# sys.path.insert(0, os.path.abspath(os.path.dirname(__file__)))
# Import and run the scheduler job
from app.scheduler import sync_all_plaid_accounts_job


async def main() -> None:
    print("Starting Plaid sync job...")
    await sync_all_plaid_accounts_job()
    print("Plaid sync job completed")


if __name__ == "__main__":
    while True:
        asyncio.run(main())
        time.sleep(300)
