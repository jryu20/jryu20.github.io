
## Question 1

## Part a)

pqnumber <- function(sign, p, q, nums) {
  # This function is the constructor function for a pqnumber
  # This will throw an error if:
  # 1) the sign argument is not the integer 1 or -1
  # 2) the p argument is not a whole number
  # 3) the q argument is not a whole number
  # 4) the length of the nums argument is not equal to p+q+1
  # 5) the entries in the nums vector are not numbers 0-9
  # Args:
  # sign: denotes the sign of the number
  # p: denotes how many digits are after the decimal point
  # q: denotes how many digits are before the units digit
  # nums: a numeric vector we use to construct the pqnumber
  # Return:
  # a pqnumber object
  
  if (abs(sign) != 1) {
    stop("The sign should be either 1 or -1.")
  }
  if (p %% 1 != 0 | q %% 1 != 0 | p < 0 | q < 0) {
    stop("p and q values must be whole numbers.")
  }
  if (length(nums) != p + q + 1) {
    stop("The length of nums must equal p+q+1.")
  }
  for (i in length(nums)) {
    if (!(nums[i] %in% 0:9)) {
      stop("The indices of nums must be an integer between 0 and 9.")
    } 
  }
  
  structure(list(sign = as.integer(sign), p = as.integer(p), q = as.integer(q), nums = as.integer(nums)), class = 'pqnumber')
}

is_pqnumber <- function(x) {
  # This function is the predicate function for a pqnumber
  # Args:
  # x: any input
  # Return:
  # a logical value checking whether the input is a pqnumber object or not
  any(class(x) == 'pqnumber')
}

print.pqnumber <- function(x, DEC = FALSE) {
  # This function is the print function for a pqnumber
  # Args:
  # x: a pqnumber
  # DEC: default set to FALSE, but if set to TRUE, will return the decimal form
  # Return:
  # either the decimal form or the object form (depends on the DEC input)
  if (DEC) {
    print_str <- paste0(substr(paste(rev(x$nums), collapse = ''), 1, x$q+1), '.', paste0(substr(paste(rev(x$nums), collapse = ''), x$q+2, length(x$nums))))
    
    if (x$sign == -1) {
      print_str <- paste0('-', print_str)
    }
  } else {
    print_str <- paste0('sign = ', x$sign, '\np = ', x$p, '\nq = ', x$q, '\nnums = ', paste(x$nums, collapse = ' '))
  }
  cat(paste0(print_str, '\n'))
}

as_pqnumber <- function(x, p, q) {
  # This function is the generic coercion function for a numeric value into a pqnumber
  # Args:
  # x: a numeric value
  # p: denotes how many digits are after the decimal point
  # q: denotes how many digits are before the units digit
  UseMethod('as_pqnumber')
}

as_pqnumber.numeric <- function(x, p, q) {
  # This function is the coercion method for the above generic function
  # Args:
  # x: a numeric value
  # p: denotes how many digits are after the decimal point
  # q: denotes how many digits are before the units digit
  # Return:
  # a pqnumber object that satisfies the given inputs
  nums <- (abs(x) * 10^seq(p, -q)) %% 10 %/% 1
  sgn <- if(x == 0) 1 else base::sign(x)
  pqnumber(sgn, p, q, nums)
}

as_numeric <- function(x) {
  # This function is the generic coercion function for a pqnumber into a numeric value
  # Args:
  # x: a pqnumber
  UseMethod('as_numeric')
}

as_numeric.pqnumber <- function(x) {
  # This function is the coercion method for the above generic function
  # Args:
  # x: a pqnumber
  # Return:
  # a numeric value that matches the pqnumber input
  if (x$sign == 1) {
    numeric <- as.numeric(paste(rev(x$nums), collapse = '')) / 10^(x$p)
  } else {
    numeric <- -1*(as.numeric(paste(rev(x$nums), collapse = '')) / 10^(x$p))
  }
  numeric
}

## Part b)

carry_over <- function(z) {
  # This helper function is the carry over function that takes care of moving digits when it needs to be moved over to the next index
  # Args:
  # z: a numeric vector
  # Return:
  # a modified vector of z that has the carry over process completed
  n <- length(z)
  carry <- 0
  for (i in 1:n) {
    zi <- z[i] + carry
    z[i] <- zi %% 10
    carry <- zi %/% 10
  }
  
  if (carry != 0) {
    z[n+1] <- carry
  }
  z
}

abs_gtr <- function(x, y) {
  # This helper function compares the absolute magnitudes of two pqnumbers
  # Args:
  # x: a pqnumber
  # y: a pqnumber
  # Return:
  # TRUE is x has a greater absolute magnitude than y, FALSE if otherwise
  if (x$p > y$p) {
    y <- pqnumber(y$sign, x$p, y$q, c(rep(0, x$p-y$p), y$nums))
  } else {
    x <- pqnumber(x$sign, y$p, x$q, c(rep(0, y$p-x$p), x$nums))
  }
  
  if (x$q > y$q) {
    y <- pqnumber(y$sign, y$p, x$q, c(y$nums, rep(0, x$q-y$q)))
  } else {
    x <- pqnumber(x$sign, x$p, y$q, c(x$nums, rep(0, y$q-x$q)))
  }
  
  x_rev <- rev(x$nums)
  y_rev <- rev(y$nums)
  gtr <- character(1)
  
  for (i in 1:length(x$nums)) {
    if (y_rev[i] > x_rev[i]) {
      gtr <- F
      break
    } else if (x_rev[i] > y_rev[i]) {
      gtr <- T
      break
    } else {
      next
    }
  }
  
  gtr
}

add <- function(x, y) {
  # This function adds two pqnumbers
  # Args:
  # x: a pqnumber
  # y: a pqnumber
  # Return:
  # the sum of x and y, returned in a pqnumber format
  max_p <- max(x$p, y$p)
  max_q <- max(x$q, y$q)
  n <- max_p + max_q + 1
  
  z <- rep(0L, n)
  if (x$sign == y$sign) {
    x_vals <- x$nums
    y_vals <- y$nums
    sgn <- x$sign
    
  } else {
    if(abs_gtr(x,y)) {
      x_vals <- x$nums
      y_vals <- -y$nums
      sgn <- x$sign
      
    } else {
      x_vals <- -x$nums
      y_vals <- y$nums
      sgn <- y$sign
    }
    
  }
  
  z[(1+max_p-x$p):(1+max_p+x$q)] <- x_vals
  z[(1+max_p-y$p):(1+max_p+y$q)] <- z[(1+max_p-y$p):(1+max_p+y$q)] + y_vals
  
  z <- carry_over(z)
  
  digit_offset <- length(z) - n
  pqnumber(sgn, max_p, max_q + digit_offset, z)
}

subtract <- function(x, y) {
  # This function subtracts one pqnumber from the other
  # Args:
  # x: a pqnumber
  # y: a pqnumber
  # Return:
  # the difference of x and y, returned in a pqnumber format
  y$sign <- y$sign * -1
  add(x, y)
}

# Part c)

multiply <- function(x,y) {
  # This function multiplies two pqnumbers
  # Args:
  # x: a pqnumber
  # y: a pqnumber
  # Return:
  # the product of x and y, returned in a pqnumber format
  
  n <- x$p + x$q + y$p + y$q + 1
  z <- rep(0L, n)
  
  for (r in 1:(1+y$p+y$q)) {
    
    x_leftover <- x$p + x$q
    z[r:(r+x_leftover)] <- z[r:(r+x_leftover)] + (x$nums*y$nums[r])
  }
  z <- carry_over(z)
  digit_offset <- length(z) - n
  sgn <- x$sign * y$sign
  pqnumber(sgn, x$p + y$p, x$q + y$q + digit_offset, z)
}
