def remove_trailing_spaces(input_file, output_file):
    with open(input_file, 'r') as infile, open(output_file, 'w') as outfile:
        for line in infile:
            outfile.write(line.rstrip() + '\n')
            
# Example usage
input_file = 'cat_taucoef.sh'
output_file = 'cat_taucoef.sh.x'
remove_trailing_spaces(input_file, output_file)
                                    
