import { Button, Slider, TextField } from '@mui/material';
import { DatePicker, LocalizationProvider } from '@mui/x-date-pickers';
import { AdapterDateFns } from '@mui/x-date-pickers/AdapterDateFns/index.js';
import { DataGrid } from '@mui/x-data-grid';

export const rawButton = Button;
export const rawSlider = Slider;
export const rawDatePicker = DatePicker;
export const datefnsAdapter = AdapterDateFns;
export const localizationProvider = LocalizationProvider;
export const rawTextField = TextField;
export const rawDataGrid = DataGrid;
