#' R6 class for simulating VARs

#' Should add a check that indicates if the given Phi and Psi results in standardized TS
simulateVAR <- R6::R6Class("simulateVAR",

    # Public attributes and methods
    public = list(

        # Initialize attributes to NULL
        Phi = NULL,
        Psi = NULL,
        num.ord.max = NULL,
        burn.in = NULL,
        max.tries = NULL,
        num_var = NULL,
        verbose = NULL,
        saved_params = NULL,
        fixed_params = FALSE,

        # Initialization method
        initialize = function(Phi, Psi,
                              num.ord.max = 7,
                              burn.in = 1000, max.tries = 100, verbose = TRUE, fixed_params = TRUE) {

            # Save inputs as attributes
            self$Phi            <- Phi
            self$Psi            <- Psi
            self$num.ord.max    <- num.ord.max
            self$burn.in        <- burn.in
            self$max.tries      <- max.tries
            self$verbose        <- verbose
            self$fixed_params   <- fixed_params

            # Initialize saved_params
            self$saved_params <- NULL

            # Extract some relevant information for later use
            self$num_var <- dim(Phi)[1]

            # Coerce arguments
            private$coerce_args()

            # Perform checks
            private$run_init_checks()

        },

        # Method to clear saved params
        clear_saved_params = function() {
            self$saved_params <- NULL
        },

        # Dummy method to initialize models
        initialize_models = function() {
            if (self$fixed_params) {
                self$clear_saved_params()
                self$generate_ts_from_model(time_len = 3000, initialize = TRUE)
            } else {
                stop("Cannot initialize models if $`fixed_params' is set to FALSE")
            }
        },


        # Method to create data in batch mode
        generate_ts_from_model = function(time_len = 100,
                                          num.ord.out = NULL,
                                          initialize = FALSE) {

            # Force users to initialize models first
            if (self$fixed_params) {
                if (!initialize && is.null(self$saved_params)) {
                    stop("Please use `$initialize_models()' to create model parameters first")
                } 
            }

            # Initialize counter
            try_num       <- 1

            # Print empty line to get rid of previous output
            cat("\n")

            # If we want to use fixed_params
            ts_created   <- FALSE
            if (self$fixed_params && !is.null(self$saved_params)) {
                curr_subj_params    <- self$saved_params
            }

            # Loop until all created or maximum number of tries reached
            while (!ts_created && try_num <= self$max.tries) {

                # Create params if we do not want to use the saved parameters or there are no saved params
                if (!self$fixed_params || is.null(self$saved_params)) {
                    # Create parameter list
                    curr_subj_params <- private$create_param_lists()
                    curr_subj_params <- curr_subj_params %>% private$init_ord_thres()
                }

                # Generate time series 
                curr_subj_params <- curr_subj_params %>%
                    private$generate_ts(time_len = time_len, num.ord.out = num.ord.out)

                # Check if params are ok
                curr_subj_params_ok <- curr_subj_params[["i0_stationary"]] && !curr_subj_params[["nan.vals.exist"]]
                if (curr_subj_params_ok) {
                    ts_created <- TRUE 
                } 

                # Log progress to console
                if (self$verbose && ts_created) {
                    if (!initialize) {
                        cat(paste("[INFO] Attempt ", try_num, " / ", self$max.tries,
                                "; Successfully generated time series.",
                                "\n",
                                sep = ""))
                    } else {
                        cat(paste("[INFO] Attempt ", try_num, " / ", self$max.tries,
                                "; Successfully initialized model parameters.",
                                "\n",
                                sep = ""))
                    }
                }

                # Update try_num
                try_num <- try_num + 1

            }


            # Message if no ts created
            if (!ts_created) {
                # Print out error
                cat(paste("[INFO] Failed to create parameters.\n"))
            }


            # Save parameters
            if (self$fixed_params && initialize) {

                    # Store parameters
                    self$saved_params <- curr_subj_params

                    # Reset the parameters that should not be saved
                    self$saved_params[["raw.ts"]]            <- NULL
                    self$saved_params[["nan.vals.exist"]]    <- NULL
                    self$saved_params[["i0_stationary"]]     <- NULL
                    self$saved_params[["ord.ts"]]            <- NULL
                    self$saved_params[["out.ts"]]            <- NULL
         
            }

            # Output
            if (!initialize) {
                return(curr_subj_params)
            }

        }


    ),

    # Private attributes and methods
    private = list(

        # Function to check if a number is a whole number
        is.wholenumber = function(x, tol = .Machine$double.eps^0.5) {
            abs(x - round(x)) < tol
        },

        # Run checks
        run_init_checks = function() {

            ## Check that the matrices are square
            Phi_checks_fail <- ( !is.matrix(self$Phi) || dim(self$Phi)[1] != dim(self$Phi)[2] )
            Psi_checks_fail <- ( !is.matrix(self$Psi) || dim(self$Psi)[1] != dim(self$Psi)[2] )
            if (Phi_checks_fail || Psi_checks_fail) {
                stop("`Phi' and `Psi' must be square matrices.")
            }

            ## Check that Psi has positive diagonals
            if (!all(diag(self$Psi) > 0)) {
                stop("`Psi' matrix must have positive diagonals.")
            }

            ## Check that num.ord is either integer or NULL (for continuous variables)
            if (!is.null(self$num.ord.max)) {
                if (!private$is.wholenumber(self$num.ord.max)) {
                stop("`num.ord.max' must be either integer-valued or NULL")
                }
            }

            ## Check if `num.ord.max' is at least 2 if specified
            if (!is.null(self$num.ord.max)) {
                if (self$num.ord.max <= 1) {
                stop("`num.ord.max' must be greater than 1.")
                }
            }

            ## Check if `Psi' matrix is positive semidefinite
            if (any(eigen(self$Psi)$values <= 0)) {
                stop("`Psi' must be positive definite.")
            }

        },

        # Coerce arguments to desired types
        coerce_args = function() {

            # Perform type coercion for the `num.time' arguments
            if(!is.null(self$num.ord.max)) {
                self$num.ord.max <- as.integer(self$num.ord.max)
            }

        },

        # Method to create parameters for N subjects
        create_param_lists = function() {

            # Initialize the matrices for each subject
            param_list_to_dup           <- vector("list", 2)
            names(param_list_to_dup)    <- c("Phi", "Psi")
            param_list_to_dup[["Phi"]]  <- self$Phi
            param_list_to_dup[["Psi"]]  <- self$Psi

            return(param_list_to_dup)

        },

        # Function to initialize the thresholds
        init_ord_thres = function(params, time_len = 3000) {

            # Generate time series 
            params <- private$generate_raw_ts(params = params, time_len = time_len)

            # Generate random thresholds based on quantiles; we use a symmetric dirichlet
            rand_unsummed_quantiles   <- extraDistr::rdirichlet(n = ncol(params[["raw.ts"]]),
                                                                alpha = rep(5, self$num.ord.max))
            person_i_q_thres          <- t(apply(rand_unsummed_quantiles, 1, cumsum))

            # Drop last column of 1s and store as list
            person_i_q_thres <- as.matrix(person_i_q_thres[,1:(ncol(person_i_q_thres)-1)])
            person_i_q_thres <- as.list(data.frame(t(person_i_q_thres)))

            # Obtain the person-specific quantiles and save
            params[["raw.thres"]] <- as.data.frame(mapply(function(x,y) {quantile(x, probs = y, names = FALSE)},
                                                          x = params[["raw.ts"]],
                                                          y = person_i_q_thres,
                                                          SIMPLIFY = FALSE))

            # Clear the generated raw time series
            params[["raw.ts"]] <- NULL

            return(params)
        },



        # Function to generate raw time series (INTERIOR USE ONLY)
        generate_raw_ts = function(params, time_len) {

            # Check inputs
            # `params' must be a list with named elements `Phi' and `Psi'
            stopifnot(all(c("Phi", "Psi") %in% names(params)))

            # Boolean to track if stationarity achieved for integrated-1 data
            person_i_stationary   <- FALSE

            # Initialize with an isotropic random gaussian
            init_i <- t(mvtnorm::rmvnorm(1, sigma = diag(self$num_var)))

            # Create container
            params[["raw.ts"]] <- matrix(data = NaN, nrow = time_len, ncol = self$num_var)

            # Loop through time
            for (time_ij in 1:(self$burn.in + time_len)) {

                # Generate random noise
                noise_y   <- t(mvtnorm::rmvnorm(1, sigma = params[["Psi"]]))

                # Apply autoregression to initial vector if time is 1
                if (time_ij == 1) {
                    y_next    <-  (params[["Phi"]] %*% init_i + noise_y)
                } else {
                    y_next    <-  (params[["Phi"]] %*% y_prev + noise_y)
                }

                # Skip storing of data if we are still in the burn.in stage
                if (time_ij > self$burn.in) {
                    params[["raw.ts"]][time_ij - self$burn.in,]  <- as.vector(y_next)
                }

                # Store the previous output for later use
                y_prev <- y_next

            }

            # Store as data.frame for the output
            params[["raw.ts"]] <- as.data.frame(params[["raw.ts"]])

            ### --- End Create Data --- ###

            # Enforce check that all values have been filled; NaNs can occur because the values explode
            # Because Inf - Inf is NaN
            contains_nan                <- (sum(is.nan(unlist(params[["raw.ts"]]))) != 0)
            params[["nan.vals.exist"]]  <- contains_nan

            # Check if the time series is stationary
            params[["i0_stationary"]]   <- private$check_integ_stationary(params[["raw.ts"]],
                                                                          params[["Phi"]])

            return(params)
        },


        # Checks if time series of stated integration order is stationary
        check_integ_stationary = function(ts_df, phi_true, max_diff = 1000) {

            # Run check only only if no NaN values found in the dataframe
            if (sum(is.nan(unlist(ts_df))) == 0) {

                # We use the actual Phi matrix to check
                all_var_stat <- all(Mod(eigen(phi_true)$values) < 1)

                # We also check if the time series blows up
                all_var_stat <- all_var_stat && max(abs(ts_df[nrow(ts_df),] - ts_df[1,])) < max_diff

            } else {
                # Set to FALSE by default
                all_var_stat <- FALSE
            }

            return(all_var_stat)
        },

        
        # Method to generate the time series data for a single person
        generate_ts = function(params, time_len, num.ord.out) {

            # Generate the raw time series
            params <- private$generate_raw_ts(params, time_len)
            
            # Discretize the observations is applicable
            if (!is.null(num.ord.out) && !params[["nan.vals.exist"]]) {

                # Discretize
                params <- private$discretize_raw_ts(params, num.ord.out)

                # Copy the relevant data to the outputs
                params[["out.ts"]] <- params[["ord.ts"]]

            } else {

                # Copy the relevant data to the outputs
                params[["out.ts"]] <- params[["raw.ts"]]

            }

            # Return the time series as a data.frame
            return(params)
        },


        # Function to discretize the time series
        discretize_raw_ts = function(params, num.ord.out) {

            # Convenience function to discretize the data
            discretize.ord <- function(x, ord.thres) {

                # NOTE: We should actually check if the thresholds are actually increasing
                if (length(ord.thres) > 1) {
                    stopifnot(ord.thres[-1] - ord.thres[-length(ord.thres)] > 0)
                }

                # Check which category the value x belongs to
                t_i <- 1
                while (ord.thres[t_i] < x && t_i <= length(ord.thres)) {
                    t_i <- t_i + 1
                }
                return(t_i)
            }
            discretize.ord <- Vectorize(discretize.ord, vectorize.args = "x")

            # Discretize based on num.ord.out (HARD-CODED; not elegant but whatever)
            thres.used <- NULL
            if (self$num.ord.max == 7) {

                # Select subset of thresholds based on output type
                if (num.ord.out == 7) {

                    # If we use 7 point scale
                    thres.used <- params[["raw.thres"]]          

                } else if (num.ord.out == 5) {
                
                    # If we use 5 point scale 
                    thres.used <- params[["raw.thres"]][c(1,2,5,6),]

                } else if (num.ord.out == 3) {

                    # If we use a 3 point scale
                    thres.used <- params[["raw.thres"]][c(1,6),]

                } else {
                
                    # Throw error if NULL encountered; this function should not be called in the first place
                    # Or invalid number of output ordinal classes
                    stop("Invalid call to discretize data OR invalid number of ordinal classes")

                }

            } else if (self$num.ord.max == 5) {

                # Select subset of thresholds based on output type
                if (num.ord.out == 5) {

                    # If we use a 5-point scale 
                    thres.used <- params[["raw.thres"]]  

                } else if (num.ord.out == 3) {

                    # If we use a 3-point scale 
                    thres.used <- params[["raw.thres"]][c(1,4),]

                } else {

                    # Throw error if NULL encountered; this function should not be called in the first place
                    # Or invalid number of output ordinal classes
                    stop("Invalid call to discretize data OR invalid number of ordinal classes")

                }

            } else if (self$num.ord.max == 3) {

                if (num.ord.out == 3) {
                    thres.used <- params[["raw.thres"]]
                } else {
                    stop("Invalid call to discretize data OR invalid number of ordinal classes")
                }

            } else {
                stop("We only accept 5-point or 7-point likert scales for num.ord.max for now")
            }

            # Apply the discretize function to all columns
            person_i_ts_ordinal <- mapply(function(x, y) {discretize.ord(x = x, ord.thres = y)},
                                          x = params[["raw.ts"]],
                                          y = thres.used,
                                          SIMPLIFY = FALSE)

            # Convert all columns to factors
            person_i_ts_ordinal <- data.frame(lapply(person_i_ts_ordinal,
                                                     factor,
                                                     ordered = TRUE,
                                                     levels = as.character(seq(1,num.ord.out))))

            # Save to params list
            params[["ord.ts"]] <- person_i_ts_ordinal

            # Return params
            return(params)
        }

    )
)