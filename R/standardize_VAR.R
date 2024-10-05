#' Creates a standardized VAR from a given stationary VAR model
#' 
standardize_VAR <- function(Phi_in, Psi_in) {

    #' Convenience function to check if Phi is stationary 
    Phi_is_stationary <- function(Phi_to_check) {
        max(Mod(eigen(Phi_to_check)$values)) < 1
    }

    #' Convenience function to check if Psi is PD
    Psi_is_pd <- function(Psi_to_check) {
        min(eigen(Psi_to_check)$values) > 0
    }

    # Check that the matrices are stationary and PD
    stopifnot(Phi_is_stationary(Phi_in))
    stopifnot(Psi_is_pd(Psi_in))

    # Compute the model-implied autocovariance
    nvars         <- dim(Phi_in)[1]
    model_autocov <- matrix(solve(diag(nvars * nvars) - Phi_in %x% Phi_in, cbind(c(Psi_in))),
                            nrow = nvars, byrow = FALSE)

    # Extract standard deviations
    model_sd <- sqrt(diag(diag(model_autocov)))

    # Create standardized Phi and Psi
    model_sd_inv <- solve(model_sd)
    new_Phi      <- model_sd_inv %*% Phi_in %*% model_sd
    new_Psi      <- model_sd_inv %*% Psi_in %*% model_sd_inv

    # Check if Phi and Psi are still stationary and PD
    stopifnot(Phi_is_stationary(new_Phi))
    stopifnot(Psi_is_pd(new_Psi))

    return(list(Phi = new_Phi, Psi = new_Psi, model_autocov = model_autocov))

}

