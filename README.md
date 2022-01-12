# CC visualization
UI framework for conformance checking result.

## Installation

python_version:3.9

numpy_version:1.21.1

pm4py_version:2.2.16

You can install the released CRAN version of modified pm4py 2.2.16 (in folder Pm4py) to:
``` r
/miniconda3/lib/python3.9/site-packages/pm4py

```
Input: an event log (.XES) and process model(.PNML)
Output: interpreter conformance checking result in multiple data visualization (dashboards)
Plugin for dashboards: Shiny by R studio 
Plugin for data visualization: PyViz Python
Plugin for conformance checking: PM4PY Python
Plugin for R Interface to Python: Reticulate by R studio 

For run program start from App.R 

For run example file !
step 1. Import model file (.xes) select input from input folder "CC-running-example.xes"
step 2. Wrapping to eventlog
step 3. Import model file (.pnml) by select input from input folder "CC-running-example.pnml"
step 4. Click Generate Visual!
