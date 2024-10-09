library(devtools)
load_all()

Phi <- bringmann_2017_dataset1()
Psi <- random_Psi(6)
sVAR_params <- standardize_VAR(Phi_in = Phi, Psi_in = Psi)

# Sanity check
standardize_VAR(Phi_in = sVAR_params$Phi, Psi_in = sVAR_params$Psi)

sVAR_sim <- simulateVAR$new(Phi = sVAR_params$Phi,
                            Psi = sVAR_params$Psi)
sVAR_sim$initialize_models()
generated_ts <- sVAR_sim$generate_ts_from_model(time_len = 1000, num.ord.out = 7)
# sapply(generated_ts$out.ts, var)


# Fit a lavaan model here!
lav_out <- fit_lavaan_VAR(generated_ts$out.ts)
lav_out$VAR_est
sVAR_sim$Phi
sVAR_sim$Psi


# Test run-simulations code here
test_simobj <- create_sim_obj(bringmann_2017_dataset1)
gen_ts <- generate_TS_from_simobj(test_simobj, num_mc_samples = 5, max_timepts = 500, num.ord.out = 3)
# lav_fit_1 <- run_simulation_one_ts(gen_ts[[1]]$ord.ts[["5"]], timepts_vec = c(50,100,200,500))


lav_fit_all <- run_simulation_multi_ts(gen_ts, timepts_vec = c(50,100,200,500))

tPhi <- test_simobj$saved_params$Phi
tPsi <- test_simobj$saved_params$Psi
extract_mc_results(lav_fit_all, tPhi, tPsi)
# compare_true_with_est(lav_fit_1$`num_tps==100`, tPhi, tPsi)

# collate_results_timelen(lav_fit_1, tPhi, tPsi)


