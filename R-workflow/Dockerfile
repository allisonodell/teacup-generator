# Dockerfile for rezviz_data_generator.R
# Generates daily reservoir conditions CSV and uploads to HydroShare
#
# Build:
#   docker build -t rezviz .
#
# Run (yesterday, default):
#   docker run --env-file .env rezviz
#
# Run for an arbitrary date:
#   docker run --env-file .env rezviz 2026-01-15
#
# Run and keep the CSV locally:
#   docker run --env-file .env -v $(pwd)/hydroshare:/app/hydroshare rezviz 2026-01-15
#
# Historical parquet files are downloaded from HydroShare during build.
# If new locations are backfilled at runtime, updated parquet files are
# uploaded to HydroShare. Rebuild the image to incorporate backfills.

FROM --platform=linux/amd64 rocker/geospatial:4.4.2

# Install R packages not included in rocker/geospatial
RUN install2.r --error --skipinstalled \
    httr2 \
    arrow \
    curl \
    jsonlite

WORKDIR /app

# Copy runtime files
COPY rezviz_data_generator.R .
COPY helper_functions.R .
COPY config/locations.geojson config/
COPY config/elevation_storage_curves.csv config/

# Create output directories
RUN mkdir -p hydroshare output

# Download latest parquet files from HydroShare during build
# HydroShare resource: 22b2f10103e5426a837defc00927afbd
# Using --fail to catch HTTP errors, -L to follow redirects
RUN curl -fL -o output/historical_baseline.parquet \
    "https://www.hydroshare.org/hsapi/resource/22b2f10103e5426a837defc00927afbd/files/historical_baseline.parquet/" && \
    curl -fL -o output/historical_statistics.parquet \
    "https://www.hydroshare.org/hsapi/resource/22b2f10103e5426a837defc00927afbd/files/historical_statistics.parquet/" && \
    echo "Validating parquet files..." && \
    head -c 4 output/historical_baseline.parquet | grep -q "PAR1" && \
    head -c 4 output/historical_statistics.parquet | grep -q "PAR1" && \
    echo "Parquet files validated successfully"

# ENTRYPOINT lets us append the date argument at docker run time
ENTRYPOINT ["Rscript", "rezviz_data_generator.R"]
