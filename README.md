# U.S. State Legislature Archives Dashboard

This interactive Shiny dashboard explores trends in state legislature archives across the United States, providing insights into the availability of **data**, **video**, and **audio** records, as well as supplementary documentation like agendas, minutes, and legislative documents.

---

## Key Highlights

- **Average years of data archives**: 22.5 years  
- **Average years of video archives**: 9 years  
- **Average years of audio archives**: 5 years  
- **Most Data Archives**: Kansas  
- **Least Data Archives**: Maryland  
- **Most Committees**: Mississippi  
- **Least Committees**: Maryland  

💡 Dive deeper into trends by using the interactive visualizations in the **Overall Trends** tab.

---

## Project Overview

### Created By  
**Inaya Rizvi**

### Advisor  
**Joseph Ferrare**

### 🎯 Purpose  
This project analyzes legislative meetings from all 50 U.S. states to better understand the transparency and accessibility of information shared by state governments. The ultimate goal is to form a foundation for building a comprehensive, queryable database on legislative data access.

### 🔍 Variables of Focus
- **Number of years of video and/or audio**  
  A core data point that indicates how accessible a state’s archives are.
  
- **Number of years of data**  
  Provides insight into how comprehensive a state’s data archiving is.

---

## Variable Explanations

### `'data'`
This encompasses all forms of archived legislative materials and gives a general idea of how far back legislative session resources go.

### `'minutes'`, `'documents'`, `'agenda'`
These fields indicate whether the corresponding resource is available:
- **Yes** = 1  
- **No** = 0  
- **Unclear** = -1  

They are coded numerically for easier analysis.

---

## 📥 Data Collection

The dataset was initially outlined by Joseph Ferrare and compiled by Inaya Rizvi through extensive research on state legislature websites. It records the availability of information shared by state legislatures regarding session meetings and public access to those records.

---

## 🛠 Technologies Used

- **R**  
- **Shiny**  
- **plotly**  
- **DT**  
- **tidyverse**  
- **readxl**  
- **shinydashboard**

---

## Link to Dashboard

https://inaya-r.shinyapps.io/StateHearings_Dashboard/ 

---

## 📎 License

MIT License – feel free to use and modify with attribution!

---

## ✨ Acknowledgements

Special thanks to:
- **Joseph Ferrare** for guidance and data structuring.
- Open legislative data sources from official state websites.

