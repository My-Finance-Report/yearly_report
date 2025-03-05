FROM python:latest

# pdf utils
RUN apt-get update && apt-get install -y poppler-utils

ENV PYTHONUNBUFFERED=1

WORKDIR /app/

# Upgrade pip and install dependencies
RUN pip install --upgrade pip

# Copy requirements file and install dependencies
COPY requirements.txt /app/
RUN pip install --no-cache-dir -r requirements.txt

# Set Python path
ENV PYTHONPATH=/app

# Copy the necessary files
COPY ./scripts /app/scripts
COPY ./pyproject.toml ./alembic.ini /app/
COPY ./app /app/app

RUN chmod +x /app/scripts/prestart.sh

# Run prestart.sh before starting FastAPI
CMD ["/bin/sh", "-c", "/app/scripts/prestart.sh && uvicorn app.main:app --host 0.0.0.0 --port 8000 --workers 2"]

# Command to start FastAPI
# CMD ["uvicorn", "app.main:app", "--host", "0.0.0.0", "--port", "8000", "--workers", "4"]
