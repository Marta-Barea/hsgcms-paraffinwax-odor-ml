# **Parffin Wax Odor detection using HS-GC/MS data combined with ML**

The **machine-learning_HSGCMS_ParaffinWax_Odor** repository contains data processing approaches and machine learning techniques applied to discriminate and quantify the odor of paraffin wax samples. The study focuses on the classification of paraffin wax based on hydroprocessing levels, which significantly affect their olfactory characteristics.

---

## 🔍 Objective

The main goal of this project is to classify paraffin wax samples into five categories based on the degree of hydroprocessing:

- **None**: Non-hydrotreated paraffin.
- **Low**: 75% hydrotreated + 25% non-hydrotreated paraffin.
- **Moderate**: 50% hydrotreated + 50% non-hydrotreated paraffin.
- **High**: 25% hydrotreated + 75% non-hydrotreated paraffin.
- **Full**: Fully hydrotreated paraffin.

---

## 🛠️ **System Requirements**

### Software
- **R version 4.2.0** 
- RStudio (optional but recommended)

1. **Hierarchical Clustering Analysis (HCA)**:
   - Performed using the `hclust` function from the **stats** package (version 4.1.2).
   - The agglomerative coefficient was calculated using the `agnes` function from the **cluster** package (version 2.1.3).
   - Results were visualized with the `fviz_dend` function from the **factoextra** package (version 1.0.7).

2. **Supervised Learning Models**:
   - Developed using the `trainControl` and `train` functions from the **caret** package (version 6.0-93).
   - Regression metrics were calculated using the **MLmetrics** package (version 1.1.1).
   - Important variables for PLS and RF models were extracted with the `varImp` function from the **caret** package.

3. **Data Visualization**:
   - Plots generated using **ggplot2** (version 3.3.6) and **graphics** (version 4.1.2).

4. **Interactive Web Application**:
   - Developed with the **shiny** package (version 1.7.2).

---

## 📂 Repository Structure

- `App/`: Contains the interactive web application developed using Shiny.
- `method development/`: Scripts and notebooks for developing data processing methods.
- `supervised algorithms/`: Scripts for supervised machine learning models.
- `unsupervised algorithms/`: Scripts for unsupervised learning models, such as clustering.
- `tis/`: Analysis scripts for Total Ion Spectra of paraffin wax samples.
- `.gitattributes` and `.gitignore`: Repository configuration files.
- `LICENSE`: Project license.
- `README.md`: This file.

---

## ⚙️ How to Use This Repository

### Clone the Repository


   <pre markdown="1"> ```bash
git clone https://github.com/Marta-Barea/hsgcms-paraffinwax-odor-ml
cd machine-learning_HSGCMS_ParaffinWax_Odor </pre>

### Running the Shiny Application
1. Place `app.R`, `svm.rds`, `svr.rds`and `test_data.xlsx` in the same folder.
2. In your R console, run: 
   
   <pre markdown="1"> ```R shiny::runApp("app.R") </pre>

3. Use the web interface to:
- 📁 **Upload** `.csv` or `.xlsx` data files.
- 🛠️ **Preprocess** data using advanced filtering techniques.
- 🤖 **Predict** hydroprocessing grades with AI.

---

### 📂 **Example Dataset**
A sample dataset (`test_data.xlsx`) is included for demonstration purposes. It contains Vis-NIR spectral readings and hydroprocessing grades for various wax samples.


---

### 🤝 **Contributors**
- **University of Cádiz (AGR-291 Research Group)**
  - Specializing in hydrocarbon characterization and spectroscopy.

---

### 📜 **License**
This project is licensed under the GNU GENERAL PUBLIC License. See `LICENSE` for details.

