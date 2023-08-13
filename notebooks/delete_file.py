# import os
# import concurrent.futures
# from tqdm import tqdm

# folder_path = '/Volumes/ExFAT2/bike_svi/data/raw/cities/London/gsv_panorama 17.51.54'

# # Count files and create list of file paths
# files = [entry.path for entry in os.scandir(folder_path) if entry.is_file()]
# num_files = len(files)

# def remove_file(file_path):
#     try:
#         os.remove(file_path)
#     except Exception as e:
#         print(f"Error while deleting file: {file_path}, Error: {str(e)}")

# # Create a ThreadPoolExecutor
# with concurrent.futures.ThreadPoolExecutor() as executor:
#     # Create a progress bar for deleting the files
#     with tqdm(total=num_files, desc="Deleting files", unit="file") as pbar:
#         futures = {executor.submit(remove_file, file_path): file_path for file_path in files}
#         for future in concurrent.futures.as_completed(futures):
#             # Update the progress bar
#             pbar.update()

import os
import concurrent.futures
from tqdm import tqdm

folder_path = '/Volumes/ExFAT2/bike_svi/data/raw/cities/London/gsv_panorama 17.51.54'

# Get the total number of items in the directory
num_items = sum(1 for _ in os.scandir(folder_path))

# Scan the directory again, this time with a progress bar
with tqdm(total=num_items, desc="Scanning files", unit="file") as pbar:
    file_list = []
    for entry in os.scandir(folder_path):
        if entry.is_file():
            file_list.append(entry.path)
        pbar.update()

# Function to remove a file and handle exceptions
def remove_file(file_path):
    try:
        os.remove(file_path)
    except Exception as e:
        print("Error while deleting file: ", file_path, " Error: ", str(e))

# Now, you can proceed to delete the files with ThreadPoolExecutor
with concurrent.futures.ThreadPoolExecutor() as executor:
    list(tqdm(executor.map(remove_file, file_list), total=len(file_list), desc="Deleting files", unit="file"))
