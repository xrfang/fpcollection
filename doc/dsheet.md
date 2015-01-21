# TDataSheet

TDataSheet is a 2-dimensional data table with visualization capability.  Its features
can be described in two categories: data manipulation and visualization.

## Data Manipulation

## Data Visualization

ctBase:
  color         |string   |#FFFFFF,#000000 |colors for background and border 
  style         |string   |-               |border style
  
ctOHLC:
  color         |string   |#008000,#FF0000 |colors for Yin, Yang and Balanced
  style         |string   |+               |style for bars, -+#
  data          |string   |1,2,3,4         |data series
  
ctLine:
  color         |string   |#000000         |color for lines
  style         |string   |*-              |style for lines 
  data          |string   |1               |data series

ctBars:
  color         |string   |#000000         |color for bars
  style         |string   |+               |style for bars, -+#
  data          |string   |1               |data series

ctScat:
  color         |string   |#000000         |color for dots
  style         |number   |*               |width for dots (.*@)
  full          |string   |1               |pannable or not
  data          |string   |1,2             |data series
