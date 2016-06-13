#### a single list of source statements to compile all the R files in the code library
#### Simon Gillings
#### Created March 2016

#include all the source files
cat('Loading all source code files...\n')
source('set_archive_path.R')
source('functions_to_set_constants_and_defaults.R')
source('functions_to_load_atlas_data.R')
source('functions_to_load_other_data.R')
source('functions_to_process_atlas_data.R')
source('functions_to_process_gridrefs.R')

