# converts dot files in info_dut directory into png files
#     and moves them into dot_output_png/ directory.

import os

path = '.'

for fname in os.listdir(path):
    basename, extension = os.path.splitext(fname)
    if extension == '.dot':
        dot_string = 'dot -Tpng ' + basename + extension + ' '
        dot_output = '-o ' + basename + '.png' 
        print dot_string + dot_output
        os.system(dot_string + dot_output)

os.system('mkdir dot_output_png')
os.system('mv *.png dot_output_png')
