import numpy as np
from osgeo import gdal
from tqdm import tqdm

# Define a function to calculate slope
def calculate_slope(DEM):
    """Calculate slope of DEM using Horn's method."""
    # Padded DEM for boundary conditions
    padded_DEM = np.pad(DEM, ((1, 1), (1, 1)), mode='edge')

    # Calculate dz/dx and dz/dy
    dzdx = (padded_DEM[2:, 1:-1] - padded_DEM[:-2, 1:-1] +
            2*(padded_DEM[2:, 2:] - padded_DEM[:-2, 2:]) +
            padded_DEM[2:, :-2] - padded_DEM[:-2, :-2]) / 8

    dzdy = (padded_DEM[1:-1, 2:] - padded_DEM[1:-1, :-2] +
            2*(padded_DEM[2:, 2:] - padded_DEM[:-2, 2:]) +
            padded_DEM[:-2, 2:] - padded_DEM[:-2, :-2]) / 8

    # Calculate slope
    slope = np.arctan(np.sqrt(dzdx**2 + dzdy**2)) * 180 / np.pi
    return slope

def dem_to_slope(path_input, path_output, chunk_size=1000):
    # Open the DEM
    ds = gdal.Open(path_input)
    band = ds.GetRasterBand(1)

    xsize = band.XSize
    ysize = band.YSize

    driver = gdal.GetDriverByName("GTiff")
    outdata = driver.Create(path_output, xsize, ysize, 1, gdal.GDT_Float32)
    outdata.SetGeoTransform(ds.GetGeoTransform())
    outdata.SetProjection(ds.GetProjection())

    num_chunks = ((xsize - 1) // chunk_size + 1) * ((ysize - 1) // chunk_size + 1)

    with tqdm(total=num_chunks, desc="Processing chunks", unit="chunk") as pbar:
        # Process the DEM in chunks
        for i in range(0, ysize, chunk_size):
            for j in range(0, xsize, chunk_size):
                # Calculate actual rows and cols to read considering overlap
                read_rows = min(chunk_size + 2, ysize - i)
                read_cols = min(chunk_size + 2, xsize - j)
                
                # Read chunk with overlap
                DEM = band.ReadAsArray(max(0, j - 1), max(0, i - 1), read_cols, read_rows).astype(float)

                # Calculate slope with overlap
                slope = calculate_slope(DEM)

                # Determine actual rows and cols to write
                write_rows = min(chunk_size, ysize - i)
                write_cols = min(chunk_size, xsize - j)
                
                # If chunk included top or left border, crop result accordingly
                start_row = 0 if i > 0 else 1
                start_col = 0 if j > 0 else 1
                
                # Write the slope array to the output, excluding the overlapping edges
                outdata.GetRasterBand(1).WriteArray(slope[start_row:start_row+write_rows, start_col:start_col+write_cols], j, i)

                pbar.update()

    outdata.GetRasterBand(1).SetNoDataValue(-9999)
    outdata.FlushCache()
    outdata = None
