# WEHI Genomics Invoicing Tool

👋 **Welcome to the team!**

This is a specialized R Shiny application designed to process genomics pricing logic and generate professional quotes. This application is driven by a standardized Master Excel Spreadsheet.

This comprehensive README is designed to help newcomers gain a complete understanding of the project **within approximately 6 hours**, enabling you to begin contributing effectively from day one.

---

## 📑 Table of Contents

[1. Quick Links](#1-quick-links)

[2. Getting Started (Start Here)](#2-getting-started-start-here)

[3. Usage](#3-usage)

[4. Project Evolution](#4-project-evolution)

[5. Guiding Principles for Interns](#5-guiding-principles-for-interns)

[6. Resources & Documentation](#6-resources--documentation)

[7. Tech Stack](#7-tech-stack)

[8. Contact](#8-contact)

---

## 1. Quick Links

* [**Live Demo**: Genomics Invoicing App](https://genomics-invoicing-demo.shinyapps.io/genomics-invoicing-25-26Summer-Test-main/)
* [**Source Code**: GitHub Repository](https://github.com/yiliyayaya/genomics-invoicing-25-26Summer-Test)

---

## 2. Getting Started (Start Here)

If you are new here, follow these steps to get the app running on your local machine.

### 2.1 Prerequisites

You generally only need **R** and **RStudio** installed. The application handles the rest.

* [**Download R**](https://cran.r-project.org/)
* [**Download RStudio**](https://posit.co/download/rstudio-desktop/)

### 2.2 Installation

1. **Clone or Download**
   ```bash
   git clone [https://github.com/yiliyayaya/genomics-invoicing-25-26Summer-Test.git](https://github.com/yiliyayaya/genomics-invoicing-25-26Summer-Test.git)
   ```

2. **Launch the App**
   Open `app.R` in RStudio. You **do not** need to manually install packages. The script contains a self-setup routine.

   Simply click **"Run App"** or run:
   ```r
   shiny::runApp()
   ```

   **Package Dependencies (Reference Only)**
   While the app installs these automatically, here is the list for manual deployment reference:
   `shiny`, `bslib`, `DT`, `dplyr`, `readxl`, `openxlsx`, `tidyr`, `rmarkdown`, `shinyjs`, `tinytex`, `RColorBrewer`

---

## 3. Usage

1. **Upload Master Sheet**
   **Crucial Step**: The app requires the "Master Spreadsheet" (.xlsx) to function. You can download the latest template directly from the app's sidebar.

2. **Select Platform & Items**
   Filter by platform and add consumables to your quote. Selecting a platform automatically redirects you to the item page.

3. **Select Services**
   Add processing services (e.g., Library Preparation). You can filter these by group.

4. **Configure & Export**
   Review the "Final Quote" tab. Set the Project Type (Internal/External) to update prices via multipliers. You can adjust quantities, apply custom discounts (percentage or value), or supplier-specific discounts.

---

## 4. Project Evolution

Understanding the history helps you appreciate why the code is written this way.

### 4.1 The Legacy Code (Previous Intake)

The previous version was a basic prototype with significant limitations:

* **Risk of Error**: Relied heavily on complex Excel formulas within the spreadsheet.
* **Limited Logic**: It could only filter by "Brand" and "Category".
* **Basic Selection**: There was no cart system; users couldn't review or edit selections.
* **Broken Export**: The "Download PDF" button existed but **did not work**.

### 4.2 The Current Code (Summer 25/26)

The new version is a comprehensive upgrade designed to match the **Genomics Team's actual workflow**.

#### Key Workflow Upgrades

1. **Platform Filtering**: Allows handling of much larger spreadsheets by filtering by platform first.
2. **Granular Selection**: Step-by-step selection for Items (filter by Protocol/Category) and Services (filter by Group).
3. **Advanced Quote Editing**: In the final step, users can remove items, change quantities, and apply bulk or specific discounts.
4. **Logic Shift**: Spreadsheet logic has been removed. The Excel file is now strictly for **Data Storage**, while all calculations are handled by the **App Code** to reduce human error.

#### Dual Export Strategy

* [**PDF Invoice**]: Designed for **External Clients**. It provides a clean summary and only displays the **Total Discount**.
* [**Excel Quote**]: Designed for **Internal Staff**. It includes a detailed breakdown showing specific **Discount Percentages and Values** for verification.

---

## 5. Guiding Principles for Interns

Guidelines to help future interns maintain and develop the project.

### 5.1 Code Design
* **Logic in Code, Not Spreadsheet**: Keep the Excel template "dumb" (data only). All pricing logic must live in the R code.
* **Minimize User Touchpoints**: Always design for the client. Reduce the number of clicks and manual inputs required to reach the final invoice.

### 5.2 Asking Questions
* **Research First**: Investigate thoroughly before asking. Can the answer be found in the code or diaries? Make reasonable assumptions to reduce the workload of the person answering.
* **Escalation Path**: Discuss with teammates -> Ask Rowland (Supervisor) -> Ask Daniela (Client) only when necessary (she is very busy).

### 5.3 Team Collaboration
* **Setup Early**: Establish a group chat in week one and confirm project meeting times with Rowland.
* **Weekly Updates**: Send a weekly update email to Rowland at least **24 hours before** your meeting.
  * [**Weekly Email Update Examples**](https://wehieduau.sharepoint.com/sites/StudentInternGroupatWEHI/Shared%20Documents/Forms/AllItems.aspx?id=%2Fsites%2FStudentInternGroupatWEHI%2FShared%20Documents%2FGenomics%20Invoicing%2F2025%2026%20Summer%2FMeeting%20%26%20Weekly%20Update%20Email%2FWeekly%20Email%20Update&viewid=afd55542%2D8e3a%2D4327%2D95f9%2D63450ae10d2a)
* **Use Jira**: Use Jira for task management to avoid duplicating work.
  * [**Our Jira Board**](https://yiliyayaya.atlassian.net/jira/software/projects/GIP22S/boards/2)

---

## 6. Resources & Documentation

### 6.1 Core Handbooks
* [**User Handbook (Online Workflow)**](https://wehieduau.sharepoint.com/:w:/r/sites/StudentInternGroupatWEHI/_layouts/15/Doc.aspx?sourcedoc=%7B1C015D32-E32B-40D4-8EB7-40192F2860C7%7D&file=User%20Handbook.docx&action=default&mobileredirect=true)
* [**Admin Handbook (Local Setup)**](https://wehieduau.sharepoint.com/:w:/r/sites/StudentInternGroupatWEHI/_layouts/15/Doc.aspx?sourcedoc=%7BC706F24B-5F4A-42C7-96ED-52BC028D93AF%7D&file=Admin%20Handbook.docx&action=default&mobileredirect=true)

### 6.2 Project Context
* [**Full Project Archive (Summer 25/26)**](https://wehieduau.sharepoint.com/sites/StudentInternGroupatWEHI/Shared%20Documents/Forms/AllItems.aspx?id=%2Fsites%2FStudentInternGroupatWEHI%2FShared%20Documents%2FGenomics%20Invoicing%2F2025%2026%20Summer&viewid=afd55542%2D8e3a%2D4327%2D95f9%2D63450ae10d2a)
* [**Project Methodology (Q&A, Wireframes)**](https://wehieduau.sharepoint.com/sites/StudentInternGroupatWEHI/Shared%20Documents/Forms/AllItems.aspx?id=%2Fsites%2FStudentInternGroupatWEHI%2FShared%20Documents%2FGenomics%20Invoicing%2F2025%2026%20Summer%2FProject%20Methodology&viewid=afd55542%2D8e3a%2D4327%2D95f9%2D63450ae10d2a)
* [**Technical Diaries**](https://wehieduau.sharepoint.com/sites/StudentInternGroupatWEHI/Shared%20Documents/Forms/AllItems.aspx?id=%2Fsites%2FStudentInternGroupatWEHI%2FShared%20Documents%2FGenomics%20Invoicing%2F2025%2026%20Summer%2FTechnical%20Diary&viewid=afd55542%2D8e3a%2D4327%2D95f9%2D63450ae10d2a)
* [**Individual Learning Plans**](https://wehieduau.sharepoint.com/sites/StudentInternGroupatWEHI/Shared%20Documents/Forms/AllItems.aspx?id=%2Fsites%2FStudentInternGroupatWEHI%2FShared%20Documents%2FGenomics%20Invoicing%2F2025%2026%20Summer%2FIndividual%20Learning%20Plans&viewid=afd55542%2D8e3a%2D4327%2D95f9%2D63450ae10d2a)

### 6.3 Historical References
* [**Previous Intake Reports (Intake 11-13)**](https://wehi-researchcomputing.github.io/intakes/)

---

## 7. Tech Stack

* **Language**: R
* **Framework**: Shiny (UI via `bslib` Bootstrap 5)
* **Exports**: `rmarkdown`, `openxlsx`, `tinytex` (LaTeX)

---

## 8. Contact

If you have any questions or run into issues, please do not hesitate to reach out. We are here to help!

For specific inquiries, please **email Rowland Mosbergen** to request the contact details for **Yixuan Zhou** or **Eason Chew**.


* **Client**: Daniela Zalcenstein
* **Supervisor**: Rowland Mosbergen
* **Interns (Summer 25/26)**: Yixuan Zhou, Eason Chew
