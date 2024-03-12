from PIL import Image
import requests
import os
from zensvi.cv import Segmenter

class SimplifiedImageTool:

    @staticmethod
    def fetch_image(pano_id, zoom, x, y):
        url_img = f'https://cbk0.google.com/cbk?output=tile&panoid={pano_id}&zoom={zoom}&x={x}&y={y}'
        try:
            image = Image.open(requests.get(url_img, stream=True).raw)
            return image
        except Exception as e:
            print(f"Failed to fetch image. Exception: {e}")
            return None

    @staticmethod
    def concat_horizontally(im1, im2):
        """
        Description of concat_horizontally
        Horizontally concatenates two images

        Args:
            im1 (undefined): first PIL image
            im2 (undefined): second PIL image

        """
        dst = Image.new('RGB', (im1.width + im2.width, im1.height))
        dst.paste(im1, (0, 0))
        dst.paste(im2, (im1.width, 0))
        return dst

    @staticmethod
    def concat_vertically(im1, im2):
        """
        Description of concat_vertically
        Vertically concatenates two images

        Args:
            im1 (undefined): first PIL image
            im2 (undefined): second PIL image

        """
        dst = Image.new('RGB', (im1.width, im1.height + im2.height))
        dst.paste(im1, (0, 0))
        dst.paste(im2, (0, im1.height))
        return dst

    @staticmethod
    def save_image(pano_id, output_folder):
        # Hardcoded values for this example
        zoom = 4
        vertical_tiles = 7
        horizontal_tiles = 13

        for x in range(horizontal_tiles):
            for y in range(vertical_tiles):
                new_img = SimplifiedImageTool.fetch_image(pano_id, zoom, x, y)
                if new_img is None:
                    continue  # Skip to next tile if image fetching fails
                if y == 0:
                    first_slice = new_img
                else:
                    first_slice = SimplifiedImageTool.concat_vertically(first_slice, new_img)
            if x == 0:
                final_image = first_slice
            else:
                final_image = SimplifiedImageTool.concat_horizontally(final_image, first_slice)

        if final_image.size[0] > 0 and final_image.size[1] > 0:
            final_image.save(f'{output_folder}/{pano_id}.png')
        else:
            raise ValueError(f"Invalid image for pano_id {pano_id}")

        return f"{pano_id}.png"

    @staticmethod
    def segment_image(input_folder, output_folder):
        segmenter = Segmenter(dataset="mapillary", task="panoptic")
        segmenter.segment(input_folder, output_folder, max_workers=4)

if __name__ == "__main__":
    # create output dir
    output_folder = "reports/images/before_after"
    os.makedirs(output_folder, exist_ok=True)
    panoid_list = [
        "sFWC7xEehg9cPFoRThdC_w", # London before
        "bFAMQU2ryF12Wlj2yTPtNw", # London after
        "e8HGLGkJbqCa8f5kwHuTtQ", # Montreal before
        "JbyV30UD3p15LXGWrEHWRA"] # Montreal after
    for panoid in panoid_list:
        if not os.path.exists(f"{output_folder}/{panoid}.png"):
            SimplifiedImageTool.save_image(panoid, output_folder)
    segment_input_folder = output_folder
    segment_output_folder = "reports/images/before_after_segmented"
    os.makedirs(segment_output_folder, exist_ok=True)
    SimplifiedImageTool.segment_image(segment_input_folder, segment_output_folder)
