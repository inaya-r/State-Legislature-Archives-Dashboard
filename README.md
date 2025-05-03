# State Legislative Archives Dashboard

This Shiny dashboard explores trends in state legislature archives across all 50 U.S. states, offering insights into the availability of data, video, and audio archives, as well as supplementary resources like agendas and meeting minutes.

🔗 **Live App**: [View the published dashboard here](https://inaya-r.shinyapps.io/StateHearings_Dashboard/)  

---

## 📊 Project Summary

This app provides a detailed overview of how much legislative information is available per state, including:

- **Average years of data archives**: 22.5 years  
- **Average years of video archives**: 9 years  
- **Average years of audio archives**: 5 years  
- **Most data archives**: Kansas  
- **Least data archives**: Maryland  
- **Most committees**: Mississippi  
- **Least committees**: Maryland

Explore detailed trends and visualizations in the "Overall Trends" section of the dashboard.

---

## 🧠 Purpose

This project analyzes publicly available legislative records from all 50 states to assess information accessibility and transparency. The overarching aim is to lay groundwork for a comprehensive database of state legislative archives.

---

## 📌 Variables of Focus

- **Number of years of video/audio**: Key metric for accessibility  
- **Number of years of data**: Indicator of archival depth  
- **Minutes, documents, agenda**: Marked as Yes (1), No (0), or Unclear (-1) based on availability

---

## 📂 Project Structure

```
.
├── data/         # Contains the cleaned and structured dataset
├── www/          # Stores static assets like images or styles
├── server.R      # Backend logic and reactive server functions
├── ui.R          # Frontend layout and input/output definitions
├── StateHearings.Rproj  # RStudio project file
```

---

## 📥 Data Collection

The dataset was created by Inaya Rizvi through research on official state legislature websites, with project guidance from advisor **Joseph Ferrare**. It captures the availability of state-level session resources including audio, video, documents, and agendas.

---

## 🛠 How to Run Locally

1. Clone this repository:
    ```bash
    git clone https://github.com/your-username/State-Legislature-Archives-Dashboard.git
    cd State-Legislature-Archives-Dashboard
    ```

2. Open `StateHearings.Rproj` in RStudio.

3. Run the app with:
    ```R
    shiny::runApp()
    ```

---

## 🧑‍💻 Author

**Inaya Rizvi**  
[LinkedIn]([#](https://www.linkedin.com/in/inaya-rizvi/))
---

## 📄 License

This project is licensed for academic and educational use only. Please contact the author for other use cases.
