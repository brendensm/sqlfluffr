# print method for zero violations

    Code
      print(result)
    Output
      No linting violations found.

# print method for violations

    Code
      print(result)
    Output
      # sqlf_lint_results: 2 violations
        line_no line_pos code  description                                    
        <int>   <int>    <chr> <chr>                                          
      1 1       1        CP01  Keywords must be consistently upper case.      
      2 1       9        LT12  Files must end with a single trailing newline. 

