# WEHI Genomics Invoicing Tool

👋 **Welcome to the team!**

This is a specialized R Shiny application designed to process genomics pricing logic and generate professional quotes. This application is driven by a standardized Master Excel Spreadsheet.

This comprehensive README is designed to help newcomers gain a complete understanding of the project **within approximately 6 hours**, enabling you to begin contributing effectively from day one.

---

## 📑 Table of Contents

[1. Quick Links](#1-quick-links)

[2. Getting Started (Start Here)](#2-getting-started-start-here)

[3. Usage](#3-usage)

[4. Codebase Guide](#4-codebase-guide)

[5. Project Evolution](#5-project-evolution)

[6. Guiding Principles for Interns](#6-guiding-principles-for-interns)

[7. Resources & Documentation](#7-resources-and-documentation)

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

1. **Upload Master Sheet (REQUIRED)** – The app requires the "Master Spreadsheet" (.xlsx) to function. You can download the latest template directly from the app's sidebar.

2. **Select Application & Protocol** – Select application and protocol to filter items and services to compatible results. Selecting a protocol automatically redirects you to the item page.

3. **Select Items** – Add items and consumables to your quote. Items can be filtered by brand and/or category.

4. **Select Services** – Add processing services (e.g., Library Preparation). Services can be filtered by group.

5. **Configure & Export** – Review the "Final Quote" tab. Set the Project Type (Internal/External) to update prices via multipliers. You can adjust quantities, apply custom discounts (percentage or value), or supplier-specific discounts.

---

## 4. Codebase Guide

### 4.1 Overview

The code in the repository is for a R-Shiny application built for the WEHI Genomics Lab to streamline the process of generating quotes for both internal and external customers for a range of usecases including grant applications, research work, etc.  
The code is currently setup such that the user would input a .xlsx master spreadsheet containing all the pricing, surcharge and discounts data and would use the R-Shiny app to interact with the data to generate invoices. 

(Note: In this section, text will be formatted as code if it is `variables` or `functions` and italicized if it is *files* and *modules*)

#### Tech Stack

* **Language**: R
* **Framework**: Shiny (UI via `bslib` Bootstrap 5)
* **Exports**: `rmarkdown`, `openxlsx`, `tinytex` (LaTeX)

### 4.2 Modules

#### File Structure
```
├── README.md                               # Overview of project and setup 
├── TestApp.R                               # Main .R file to launch the application 
├── requirements/ 
│   └── requirements.R                      # Dependencies and libraries checking 
├── assets/                                 # Folder to store images and digital assets
└── src/ 
    ├── server.R                            # Main backend logic (Shiny server) 
    ├── ui.R                                # Main frontend layout (Shiny UI) 
    ├── server-files/ 
    │   ├── data-processing.R               # Logic for reading .xlsx and data cleaning 
    │   ├── application-protocol-select.R   # Logic for selecting application + protocol (Page 1) 
    │   ├── item-select.R                   # Logic for items select table (Page 2) 
    │   ├── charges-select.R                # Logic for processing charges select (Page 3) 
    │   ├── final-quote.R                   # Logic for quote summary and discounts (Page 4) 
    │   └── output.R                        # Logic for generating .xlsx and .pdf outputs 
    └── ui-files/ 
        ├── application-protocol-ui.R       # UI structure for application + protocol select page (Page 1) 
        ├── item-ui.R                       # UI structure for item select page (Page 2) 
        ├── charges-ui.R                    # UI structure for processing charges select page (Page 3) 
        └── final-quote-ui.R                # UI structure for quote summary and discounts (Page 4) 
```

#### Code Structure Diagram
![Modules structure diagram](assets/module-diagram.png)
This diagram is up to date as of 12/1/2026. Please review main repository structure to confirm.

#### Detailed Variable descriptions
- `values (reactiveValues)` - A list of reactive values that stores all data which relies on user inputs or data processed from the master spreadsheet. It is declared in *TestApp.R* .
   - `values$application_select (string)` – A string storing the selected Application
   - `values$protocol_select (string)` – A string storing the selected Protocol
   - `values$cart (dataframe)` – A dataframe which stores all items and charges selected by the user.
   - `values$data (list)` – A list of values containing cleaned data using functions from *src/server-files/data-processing.R*. Function call to generate this list is called in src/server.R using `process_pricing_logic()`. 
      - `data$items (dataframe)` – A dataframe containing the items read from price list (Page 1) of the master spreadsheet. 
      - `data$services (dataframe)` – A dataframe containing the services read from processing charges (Page 2) of the master spreadsheet. 
      - `data$logic_proc (dataframe)` – A dataframe containing the surcharge amounts to be applied to services from config (Page 3) of the master spreadsheet. 
      - `data$logic_item (dataframe)` – A dataframe containing the surcharge amounts to be applied to items from config (Page 3) of the master spreadsheet. 
      - `data$application_protocol_item (dataframe)` – A dataframe mapping the relation for price list items between Brand, Item, Application and Protocol. This is necessary because the same Item can come from multiple brands. 
      - `data$application_protocol_proc (dataframe)` – A dataframe mapping the relation for processing charges between Service, Application and Protocol. This is used for filtering services later. 
      - `data$supplier_discount (dataframe)` – A dataframe containing the supplier discounts listed in the master spreadsheet (Page 4). 

#### Detailed File descriptions
*TestApp.R* - This file is reserved only for calling the driver functions from *server.R*, *ui.R* and *requirements.R*. Changes to this file should me minimized/avoided to ensure a clear separation of frontend and backend logic. All common variables used in the program are initialized here in `values <- reactiveValues()`. 

*requirements/requirements.R* - This file is used to store functions that check for installed libraries and installs them if not already installed. Any new libraries can be added to vector `REQUIRED_PACKAGES` without changing the main program. 

*src/server.R* - The file storing the main logic for server function of R-Shiny app. All observers and processing logic is called here. All select lists that need to be populated using data from the master spreadsheet can be added to the `populate_select_lists()` function. Functions for each page will be stored in their respective module files in *src/server-files*. All observers are declared in this file.

*src/ui.R* - This file structures the app’s UI consisting of sidebar features and calling individual functions to generate the `nav_panel`(main section) for each page. All files in *src/ui-files* have the same structure, 1 function to generate the main content panel for that page and another to generate the the relevant sidebar content. 

### 4.3 Code Guidelines
Here, we will outline the expectation for how code should be written throughout this program. It is important that the guidelines are followed to improve maintainability and readability of the program.

#### Naming
Variables should be named based on either what the data is containing or the intended use of the variable. Generic variable names such as `var` or `df` should be avoided.

If the variable name is longer than 1 word, use underscores ( \_ ) to represent spaces in variable names. Function and parameter naming also follow the same conventions.
```r
df <- spreadsheet_data[1] # Vague names/labels should be avoided ❌
items_pricing_data <- spreadsheet_data[1] # Clear naming of data ✅
```

#### Functions

The function name should provide a clear description of the purpose or use case of the function. Each function should have its scope focused on completing a single task/purpose. Each function should have an accompanying docstring at the top of the function definition using # as shown below.
```r
function_name <- function(parameter1, parameter2) {
   # Function description and explanation goes here
   #
   # Parameters:
   # parameter1 (parameter1 data type) - Description of parameter1
   # parameter2 (parameter2 data type) - Description of parameter2

   # Code and logic goes here...

   return(cleaned_data) # Explicit return
}
```

If the function is expected to return a value, it must be explicitly stated using the `return()` function rather than rely on R's automatic return to improve readability. Functions without an explicit `return()` are assumed to return void and return value will be ignored.

---

## 5. Project Evolution

Understanding the history helps you appreciate why the code is written this way.

### 5.1 The Legacy Code (Previous Intake)

The previous version was a basic prototype with significant limitations:

* **Risk of Error**: Relied heavily on complex Excel formulas within the spreadsheet.
* **Limited Logic**: It could only filter by "Brand" and "Category".
* **Basic Selection**: There was no cart system; users couldn't review or edit selections.
* **Broken Export**: The "Download PDF" button existed but **did not work**.

### 5.2 The Current Code (Summer 25/26)

The new version is a comprehensive upgrade designed to match the **Genomics Team's actual workflow**.

#### Key Workflow Upgrades

1. **Application + Protocol Filtering**: Allows handling of much larger spreadsheets by filtering by application and protocol first.
2. **Granular Selection**: Step-by-step selection for Items (filter by Protocol/Category) and Services (filter by Group).
3. **Advanced Quote Editing**: In the final step, users can remove items, change quantities, and apply bulk or specific discounts.
4. **Logic Shift**: Spreadsheet logic has been removed. The Excel file is now strictly for **Data Storage**, while all calculations are handled by the **App Code** to reduce human error.

#### Dual Export Strategy

* [**PDF Invoice**]: Designed for **External Clients**. It provides a clean summary and only displays the **Total Discount**.
* [**Excel Quote**]: Designed for **Internal Staff**. It includes a detailed breakdown showing specific **Discount Percentages and Values** for verification.

---

## 6. Guiding Principles for Interns

Guidelines to help future interns maintain and develop the project.

### 6.1 Code Design
* **Logic in Code, Not Spreadsheet**: Keep the Excel template "dumb" (data only). All pricing logic must live in the R code.
* **Minimize User Touchpoints**: Always design for the client. Reduce the number of clicks and manual inputs required to reach the final invoice.

### 6.2 Asking Questions
* **Research First**: Investigate thoroughly before asking. Can the answer be found in the code or diaries? Make reasonable assumptions to reduce the workload of the person answering.
* **Escalation Path**: Discuss with teammates -> Ask Rowland (Supervisor) -> Ask Daniela (Client) only when necessary (she is very busy).

### 6.3 Team Collaboration
* **Setup Early**: Establish a group chat in week one and confirm project meeting times with Rowland.
* **Weekly Updates**: Send a weekly update email to Rowland at least **24 hours before** your meeting.
  * [**Weekly Email Update Examples**](https://wehieduau.sharepoint.com/sites/StudentInternGroupatWEHI/Shared%20Documents/Forms/AllItems.aspx?id=%2Fsites%2FStudentInternGroupatWEHI%2FShared%20Documents%2FGenomics%20Invoicing%2F2025%2026%20Summer%2FMeeting%20%26%20Weekly%20Update%20Email%2FWeekly%20Email%20Update&viewid=afd55542%2D8e3a%2D4327%2D95f9%2D63450ae10d2a)
* **Use Jira**: Use Jira for task management to avoid duplicating work.
  * [**Our Jira Board**](https://yiliyayaya.atlassian.net/jira/software/projects/GIP22S/boards/2)

---

## 7. Resources & Documentation

### 7.1 Core Handbooks
* [**User Handbook (Online Workflow)**](https://wehieduau.sharepoint.com/:w:/r/sites/StudentInternGroupatWEHI/_layouts/15/Doc.aspx?sourcedoc=%7B1C015D32-E32B-40D4-8EB7-40192F2860C7%7D&file=User%20Handbook.docx&action=default&mobileredirect=true)
* [**Admin Handbook (Local Setup)**](https://wehieduau.sharepoint.com/:w:/r/sites/StudentInternGroupatWEHI/_layouts/15/Doc.aspx?sourcedoc=%7BC706F24B-5F4A-42C7-96ED-52BC028D93AF%7D&file=Admin%20Handbook.docx&action=default&mobileredirect=true)

### 7.2 Project Context
* [**Full Project Archive (Summer 25/26)**](https://wehieduau.sharepoint.com/sites/StudentInternGroupatWEHI/Shared%20Documents/Forms/AllItems.aspx?id=%2Fsites%2FStudentInternGroupatWEHI%2FShared%20Documents%2FGenomics%20Invoicing%2F2025%2026%20Summer&viewid=afd55542%2D8e3a%2D4327%2D95f9%2D63450ae10d2a)
* [**Project Methodology (Q&A, Wireframes)**](https://wehieduau.sharepoint.com/sites/StudentInternGroupatWEHI/Shared%20Documents/Forms/AllItems.aspx?id=%2Fsites%2FStudentInternGroupatWEHI%2FShared%20Documents%2FGenomics%20Invoicing%2F2025%2026%20Summer%2FProject%20Methodology&viewid=afd55542%2D8e3a%2D4327%2D95f9%2D63450ae10d2a)
* [**Technical Diaries**](https://wehieduau.sharepoint.com/sites/StudentInternGroupatWEHI/Shared%20Documents/Forms/AllItems.aspx?id=%2Fsites%2FStudentInternGroupatWEHI%2FShared%20Documents%2FGenomics%20Invoicing%2F2025%2026%20Summer%2FTechnical%20Diary&viewid=afd55542%2D8e3a%2D4327%2D95f9%2D63450ae10d2a)
* [**Individual Learning Plans**](https://wehieduau.sharepoint.com/sites/StudentInternGroupatWEHI/Shared%20Documents/Forms/AllItems.aspx?id=%2Fsites%2FStudentInternGroupatWEHI%2FShared%20Documents%2FGenomics%20Invoicing%2F2025%2026%20Summer%2FIndividual%20Learning%20Plans&viewid=afd55542%2D8e3a%2D4327%2D95f9%2D63450ae10d2a)

### 7.3 Historical References
* [**Previous Intake Reports (Intake 11-13)**](https://wehi-researchcomputing.github.io/intakes/)

---

## 8. Contact

If you have any questions or run into issues, please do not hesitate to reach out. We are here to help!

For specific inquiries, please **email Rowland Mosbergen** to request the contact details for **Yixuan Zhou** or **Eason Chew**.


* **Client**: Daniela Zalcenstein
* **Supervisor**: Rowland Mosbergen
* **Interns (Summer 25/26)**: Yixuan Zhou, Eason Chew
