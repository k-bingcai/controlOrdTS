#' Creates a standardized VAR from a given stationary VAR model
#' 
standardize_VAR <- function(Phi_in, Psi_in) {

    # Check that the matrices are stationary and PD

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

    return(list(Phi = new_Phi, Psi = new_Psi, model_autocov = model_autocov))

}