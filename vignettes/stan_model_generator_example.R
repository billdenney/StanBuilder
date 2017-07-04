full_program <- stan_program(blocks=list())

full_program <-
  full_program +
  stan_block(block_type="data",
             contents=list(
               stan_primitive(varname="J",
                              type="int",
                              range=list(lower=0)),
               stan_array(object=stan_primitive(varname="y",
                                                type="real"),
                          size="J"),
               stan_array(object=stan_primitive(varname="sigma",
                                                type="real",
                                                range=list(lower=0)),
                          size="J"))) +
  stan_block(block_type="parameters",
             contents=list(
               stan_primitive(varname="mu",
                              type="real"),
               stan_primitive(varname="tau",
                              type="real",
                              range=list(lower=0)),
               stan_vector(varname="eta",
                           size="J"))) +
  stan_block(block_type="transformed parameters",
             contents=list(
               stan_vector(varname="theta",
                           size="J"),
               "theta = mu + tau * eta")) + # Expressions are currently just character vectors
  stan_block(block_type="model",
             contents=list(
               "target += normal_lpdf(eta | 0, 1)",
               "target += normal_lpdf(y | theta, sigma)"))
