# expand_bin Function Comparison: Old vs New Approach

## Overview

This document compares the original `expand_bin` function with the new `expand_bin_hesper` method designed specifically for HesperVector objects.

## Original expand_bin Function

### Design Philosophy
- **Data.frame-centric**: Designed to work with data.table/data.frame objects
- **String splitting**: Assumes variables contain concatenated values separated by delimiters
- **In-place modification**: Modifies the input data.frame by adding binary columns
- **Complex logic**: Handles multiple edge cases for data.frame manipulation

### Function Signature
```r
expand_bin(
  df,
  vars,
  split_by = " ",
  bin_sep = ".",
  drop_undefined = NULL,
  value_in = NULL,
  value_in_suffix = NULL,
  remove_new_bin = TRUE,
  remove_other_bin = TRUE
)
```

### Key Features
- Takes a data.frame and variable names
- Splits string values by separator
- Creates binary columns in the data.frame
- Handles undefined values
- Complex column management (removal, replacement)
- Returns modified data.frame

### Limitations for HesperVector Use
1. **Wrong abstraction level**: Operates on data.frames, not on individual vectors
2. **String-based**: Assumes values are concatenated strings needing splitting
3. **Side effects**: Modifies input data.frame structure
4. **Not object-oriented**: Doesn't leverage S7 class system
5. **Complex interface**: Many parameters for data.frame manipulation
6. **Data.table dependency**: Requires data.table for operation

## New expand_bin_hesper Method

### Design Philosophy
- **Object-oriented**: Designed specifically for HesperVector objects
- **Vector-centric**: Works with discrete values, not concatenated strings
- **Functional**: Returns new data without modifying input
- **Simple and focused**: Single responsibility of creating binary indicators

### Function Signature
```r
expand_bin_hesper(
  hesper_vector,
  include_all_opts = TRUE,
  prefix = NULL,
  separator = "_"
)

# S7 method
expand_bin(object, ...)
```

### Key Features
- Takes a HesperVector object
- Works with discrete values (no string splitting needed)
- Returns a list of binary vectors
- Handles missing values based on allow_missing property
- Simple, focused interface
- Integrates with S7 class system

### Advantages
1. **Type safety**: Works specifically with HesperVector objects
2. **Cleaner interface**: Fewer parameters, more intuitive
3. **No side effects**: Returns new data, doesn't modify input
4. **Better abstraction**: Matches the HesperVector concept
5. **Flexible output**: Returns list that can be used in various ways
6. **Metadata preservation**: Includes attributes with useful information

## Comparison Table

| Aspect | Original expand_bin | New expand_bin_hesper |
|--------|-------------------|---------------------|
| **Input Type** | data.frame + variable names | HesperVector object |
| **Output Type** | Modified data.frame | List of binary vectors |
| **Data Processing** | String splitting | Direct value comparison |
| **Side Effects** | Modifies input data.frame | Pure function |
| **Type Safety** | Generic data.frame function | HesperVector-specific |
| **Complexity** | High (200+ lines) | Low (~170 lines) |
| **Dependencies** | data.table required | Base R + checkmate |
| **S7 Integration** | None | Full S7 method support |
| **Missing Value Handling** | Through drop_undefined | Built-in with allow_missing |
| **Naming Convention** | Complex with multiple options | Simple prefix + separator |

## Usage Examples

### Original Function
```r
# Assuming data.frame with concatenated values
df <- data.frame(
  hesper_water = c("serious_problem no_serious_problem", "dnk", "serious_problem"),
  id = 1:3
)

# Expand to binary columns
df_expanded <- expand_bin(df, "hesper_water", split_by = " ")
# Result: data.frame with additional binary columns
```

### New Method
```r
# Create HesperVector
hv <- HesperVector(
  hesper_var = "hesper_drinking_water",
  hesper_vals = c("serious_problem", "dnk", "serious_problem"),
  allow_missing = FALSE
)

# Expand to binary indicators
binary_list <- expand_bin(hv)
# Result: Named list of binary vectors
```

## Migration Guide

### For Users of Original expand_bin

1. **Data Structure Change**:
   - Old: Work with data.frames containing string variables
   - New: Create HesperVector objects first

2. **Output Handling**:
   - Old: Binary columns added to data.frame
   - New: List of binary vectors returned

3. **Integration**:
   ```r
   # Old approach
   df <- expand_bin(df, vars)
   
   # New approach
   hv <- HesperVector(hesper_var = "var", hesper_vals = df$var)
   binary_list <- expand_bin(hv)
   # Convert back to data.frame if needed
   binary_df <- data.frame(binary_list)
   ```

## Recommendations

### Use New expand_bin_hesper When:
- Working with HesperVector objects
- Need clean, functional programming approach
- Want type safety and better error handling
- Building S7-based workflows
- Need flexible output format

### Consider Original expand_bin When:
- Working with legacy data.frame workflows
- Need to process multiple variables simultaneously
- Have existing code dependent on data.table modifications
- Working with concatenated string data that needs splitting

## Future Considerations

1. **Backwards Compatibility**: Keep original function for legacy support
2. **Performance**: New method may be faster for single vectors
3. **Integration**: New method fits better with overall hespeR package design
4. **Maintenance**: Simpler code in new method easier to maintain and extend

## Conclusion

The new `expand_bin_hesper` method represents a significant improvement in design philosophy, moving from generic data.frame manipulation to object-oriented, type-safe operations specifically designed for HESPER data structures. While the original function remains useful for certain use cases, the new method should be preferred for new HesperVector-based workflows.