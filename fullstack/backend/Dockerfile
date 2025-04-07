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

# Make scripts executable
RUN chmod +x /app/scripts/prestart.sh /app/scripts/start-prod.sh


# Run prestart.sh and start FastAPI with logging in production
CMD ["/app/scripts/start-prod.sh"]
