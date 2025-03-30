
# Demo of using the simulation code (script 05) to estimate effects
# easily on a given dataset.
#
# Dataset does need to be in the matrix format. 


# POSSIBLE TODO: Make code to create the datasets in this format more
# easily?


# Get the functions we need!
source( "simulation_pipeline/05_generate_predicted_IATE.R" )

# get some testing data
source( "tests/create_testing_data.R" )


# Predict IATEs on the validation set using the training set.
res <- generate_predicted_IATE( y_tr = y_tr,
                                d_tr = d_tr,
                                x_tr = x_tr, 
                                x_val = x_val,
                                model_list = c( "OLS S", "BART T", "RF T", 
                                                "LASSO T", "SL T" ),
                                #model_list = c( "OLS S", "ATE", "LASSO T" ),
                                verbose = 1000 )

names(res)

head( res$iate )
summary( res$iate )

head( res$runtime )

res$warnings

print( 100 * apply( res$fail_rate, 2, mean ) )


