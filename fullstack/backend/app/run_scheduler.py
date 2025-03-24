#!/usr/bin/env python3
"""
Script to run the Plaid sync scheduler
"""
import os
import sys
import asyncio
import time

# Add the current directory to the Python path
#sys.path.insert(0, os.path.abspath(os.path.dirname(__file__)))

# Import and run the scheduler job
from app.scheduler import sync_all_plaid_accounts_job

async def main():
    print("Starting Plaid sync job...")
    await sync_all_plaid_accounts_job()
    print("Plaid sync job completed")

if __name__ == "__main__":
    asyncio.run(main())

    # sleep for 10 minutes
    #time.sleep(600)
        
    
