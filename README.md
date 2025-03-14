# Immunoblot Analysis Tool

## Overview
The **Immunoblot Analysis Tool** is a Shiny-based application designed to facilitate the quantification and statistical analysis of immunoblot images. It provides an intuitive interface for image preprocessing, lane selection, and automated statistical analysis of protein expression data. The application allows users to upload immunoblot images, adjust their visualization, select regions for quantification, normalize intensity values, and perform statistical comparisons across experimental groups.

## Features
- **Image Processing:** Upload and preprocess immunoblot images with options to rotate, adjust brightness/contrast, and convert to grayscale.
- **Lane Selection:** Define lane regions for intensity quantification and apply background correction.
- **Data Analysis:** Upload CSV data, normalize intensity values, and select appropriate statistical tests.
- **Statistical Comparisons:** Automatically recommend the best test (e.g., t-test, ANOVA, Kruskal-Wallis) and apply pairwise or overall comparisons.
- **Customizable Plots:** Modify plot aesthetics including color schemes, text sizes, and significance labels.
- **Data and Plot Export:** Download quantified lane intensity data and analysis plots in various formats.

## Web Access
This app can we accessed through the web at https://019595f7-403a-1cce-f8bf-33f376dfe4d9.share.connect.posit.cloud/

## Usage
### 1. Image Quantification
- **Upload Image:** Choose an immunoblot image file (PNG, JPEG, or TIFF).
- **Preprocess Image:** Use options to mirror, rotate, adjust brightness/contrast, and apply grayscale conversion.
- **Zoom & Centering:** Adjust the zoom level and reposition the image view.
- **Select Lanes:** Define lane regions for quantification and set a background region for normalization.
- **Download Data:** Export the quantified intensity values as a CSV file.

### 2. Statistical Analysis
- **Upload Data:** Provide a CSV file containing protein expression data.
- **Column Selection:** Specify which columns correspond to group labels, protein expression, and loading control values.
- **Normalization:** Choose a control group for normalization of protein expression.
- **Test Recommendation:** Click the "Recommend Best Test" button to determine the most appropriate statistical test.
- **Run Analysis:** Perform statistical comparisons across groups and visualize results.
- **View Results:** Display statistical test results in a formatted table.

### 3. Plot Customization
- **Modify Aesthetics:** Adjust title, axis labels, legend, font sizes, and colors.
- **Reorder Groups:** Customize the group order for the x-axis.
- **Select Theme:** Choose from various ggplot themes for visualization.
- **Significance Labels:** Adjust the text size of significance markers.
- **Update & Export:** Apply changes and download customized plots.

## Outputs
- **Regions Data CSV:** Contains lane intensities after background correction.
- **Statistical Test Table:** Displays p-values and test results.
- **Plots:** Export analysis plots as PNG or RDS files.

## Author
**Created by Andy Ring**  
**Version:** 1.0.0  
**Date:** March 14th, 2025

