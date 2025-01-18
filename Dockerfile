# Use the Haskell base image
FROM haskell:latest

# Add the PostgreSQL APT repository and install dependencies
RUN apt-get update && apt-get install -y wget gnupg && \
    wget --quiet -O - https://www.postgresql.org/media/keys/ACCC4CF8.asc | apt-key add - && \
    echo "deb http://apt.postgresql.org/pub/repos/apt/ bullseye-pgdg main" > /etc/apt/sources.list.d/pgdg.list && \
    apt-get update && apt-get install -y \
    postgresql-14 \
    libpq-dev \
    postgresql-client-14 \
    pkg-config \
    zlib1g-dev \
    poppler-utils

# Set the working directory
WORKDIR /app

# Copy only the stack configuration files first
COPY stack.yaml stack.yaml.lock year-report-finances.cabal /app/

RUN stack setup --install-ghc
RUN stack build --only-dependencies

# Copy the rest of the application files
COPY src/ /app/src/
COPY static/ /app/static/

# Build the application
RUN stack build

# Expose the application port
EXPOSE 8080

# Run the application
CMD ["stack", "exec", "year-report-finances"]
