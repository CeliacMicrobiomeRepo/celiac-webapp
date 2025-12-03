"""
Fetches the latest dataset files from the Celiac Microbiome Repository (CMR).

This script downloads the core dataset files from the CMR GitHub repository that contain
information about included and excluded samples and datasets. It safely updates local copies
by first ensuring all downloads are successful before replacing any existing files.

Files downloaded:
 - `included_datasets.tsv` - Contains all datasets that were included in the current version.
 - `excluded_datasets.tsv` - Contains all datasets/studies that were not able to be included despite being eligible.
 - `all_samples.tsv`       - Contains all samples in the datasets in `included_datasets.tsv`.
 - `low_read_samples.tsv`  - Contains all samples in `all_samples.tsv` with final read counts after DADA2 less than 1,000.
"""


import requests
from pathlib import Path

# GitHub raw content base URL for the repository
BASE_URL = "https://raw.githubusercontent.com/CeliacMicrobiomeRepo/celiac-repository/main/"

# List of files to download
FILES_TO_FETCH = [
    "all_samples.tsv",
    "excluded_datasets.tsv",
    "included_datasets.tsv",
    "low_read_samples.tsv"
]

def download_file(url, target_path):
    """
    Download a file from a URL and return True if successful, False otherwise.
    """
    try:
        response = requests.get(url, timeout=30)
        response.raise_for_status()  # Raises an HTTPError for bad responses (4xx, 5xx)
        
        # Write to a temporary file first
        temp_path = target_path.with_suffix('.tmp')
        with open(temp_path, 'wb') as f:
            f.write(response.content)
            
        # If we got here, the download was successful
        return temp_path
    except (requests.RequestException, IOError) as e:
        print(f"Error downloading {url}: {str(e)}")
        if 'temp_path' in locals():
            temp_path.unlink(missing_ok=True)
        return None

def main():
    # Get the script's directory
    script_dir = Path(__file__).parent.absolute()
    
    # Dictionary to track successful downloads
    successful_downloads = {}
    
    # First, download all files to temporary locations
    for filename in FILES_TO_FETCH:
        url = BASE_URL + filename
        target_path = script_dir / filename
        print(f"Downloading {filename}...")
        
        temp_path = download_file(url, target_path)
        if temp_path:
            successful_downloads[filename] = temp_path
        else:
            print(f"Failed to download {filename}")
            # Clean up any temporary files from successful downloads
            for temp_file in successful_downloads.values():
                temp_file.unlink(missing_ok=True)
            return
    
    # If we got here, all downloads were successful
    # Now we can safely replace the existing files
    for filename, temp_path in successful_downloads.items():
        target_path = script_dir / filename
        if target_path.exists():
            print(f"Removing existing {filename}")
            target_path.unlink()
        temp_path.rename(target_path)
        print(f"Successfully updated {filename}")

if __name__ == "__main__":
    main()
