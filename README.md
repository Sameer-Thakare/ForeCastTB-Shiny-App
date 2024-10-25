# forecastTB Shiny App

`forecastTB` is a Shiny application designed to simplify time series forecasting. With `forecastTB`, users can upload their time series data and create forecasts using various customizable parameters. This app provides an interactive and user-friendly interface, making it easier for data scientists, analysts, and business users to analyze and project time-based data.

## Table of Contents
- [Features](#features)
- [Usage](#usage)
- [Parameters](#parameters)
- [Contributing](#contributing)
- [License](#license)

## Features

- **Data Upload**: Upload your time series data in CSV format.
- **Forecast Options**: Select from a variety of forecasting models and parameters.
- **Interactive Visuals**: View the original data alongside forecasted results.
- **Export Results**: Download forecast data in CSV format for further analysis (not yet implemented).

## Usage

1. **Launch the app**:
   Open browser, and enter the link 'https://psfonline.shinyapps.io/ForeCastTB/'.

2. **Upload Data**:
   - Go to the **Dataset** tab.
   - Browse and upload your CSV file with time series data. The app supports files with a `Date` column and a `Value` column for easy parsing.
   - You can see the entered dataset and summary of it after selecting the dataset.
   - A default dataset 'nottem.csv'is also available for testing.

3. **Forecast**:
   - Switch to the **Forecast** tab.
   - Choose your preferred forecasting method and adjust parameters like prediction parameter, number of predictions, length of data subset and others as applicable. 
   
4. **View Results**:
   - The results will be available as soon as you select all the parameters.
   - It will show
       1) Graph of 'Error Values' and 'Execution Time'.
       2) Graph of 'Forecasted values'.
       3) Numerical results and polar plot are also available for easy analysis.
   - View the original vs. forecasted data in an interactive plot.

## Parameters

- **Method Selection**: Choose from methods such as ARIMA, PSF and LPSF.
- **Prediction Parameter**: Select the parameter which you want to predict.
- **Number of Predictions**: Select number of predictions.
- **Lenght of Data Subset**: Select thr length of data subset.
- **Srategies**: Select Either 'Recursive' or 'DirRec'.

## Contributing

Contributions are welcome! Please submit a pull request or open an issue for feature requests, bug reports, or suggestions.

## License

This project is licensed under the MIT License.
