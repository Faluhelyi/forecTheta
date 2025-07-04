
## included in version 2.7.1
#' Statistical test for seasonal behavior
#' @aliases seasonal_test
#' @param y Object of time series class
#' @param s_test If \code{"default"} time series is tested for statistically seasonal behaviour. If \code{"unit_root"}, then First a difference is taken if a unit root is detected.
#' @return A logical result indicating whether or not seasonal behavior was detected.
#' @references
#' Fiorucci J.A., Pellegrini T.R., Louzada F., Petropoulos F., Koehler, A. (2016). Models for optimising the theta method and their relationship to state space models, International Journal of Forecasting, 32 (4), 1151–1161, <doi:10.1016/j.ijforecast.2016.02.005>.
#' Assimakopoulos, V. and Nikolopoulos k. (2000). The theta model: a decomposition approach to forecasting. International Journal of Forecasting 16, 4, 521–530, <doi:10.1016/S0169-2070(00)00066-2>.
#' @details
#' By default, the 90\% significance seasonal Z-test, used by Assimakopoulos and Nikolopoulos (2000), is applied for quarterly and monthly time series.
#' The possibility of first checking the unit root was included because it was pointed out that this test presents many flaws for time series with this characteristic (Fiorucci et al, 2016)).
#' In this case, the KPSS test is performed with a significance level of 5\% and in the case of a unit root, then the series is differentiated before checking for seasonal behavior.
#' @examples
#' \donttest{
#' seasonal_test(AirPassengers)
#' seasonal_test(AirPassengers, "unit")
#' }
#' @export
seasonal_test = function(y, s_test = c("default","unit_root")){

  fq = frequency(y)

  run_s_decomp = FALSE
  if( fq >= 3 ){

    if( "logical" %in% class(s_test)){
      run_s_decomp = s_test
    }else{
      s_test = match.arg(arg=s_test, choices=c("default","unit_root"))

      yy = y
      if( s_test == "unit_root" ){
        if( kpss.test(y)$p.value < 0.05){
          yy = diff(y)
        }
      }

      xacf = acf(yy, lag.max=fq+1, plot = FALSE)$acf[-1, 1, 1]
      clim = 1.64/sqrt(length(yy)) * sqrt(cumsum(c(1, 2 * xacf^2)))
      run_s_decomp = abs(xacf[fq]) > clim[fq]

      s_test = paste0(s_test,": ", run_s_decomp)

      rm(yy)
    }

  }

  return( run_s_decomp )
}


## compute quantiles according to the level,
## as level=c(80, 90) will compute lo 80, Hi 80, Lo 90, Hi 90
#
# bs_series: boostrap series distruted in columns
# level: a vector with covers probabilities
bootstrap_quantiles <- function( bs_series, level ){

  nn = length(level)
  qq = (1-0.01*level)/2
  probs = numeric(2*nn)
  probs[2*(1:nn)-1] = qq
  probs[2*(1:nn)] = 1-qq

  quantiles = t( apply(X=bs_series, MARGIN=1, FUN=quantile, probs=probs) )
  colnames(quantiles) = unlist( lapply(X=1:nn, FUN=function(X) paste(c('Lo','Hi'), level[X]) ) )

  return( quantiles )
}


## par = c(ell0, alpha, theta)
## s_type = c("additive","multiplicative","stl")
## s_test = c("default","unit_root",TRUE, FALSE)

twoTL <- function(y, h, level,
                  s_type, ## s_type = c("additive","multiplicative","stl")
                  s_test, ## s_test = c("default","unit_root",TRUE, FALSE)
                  par_ini, estimation, lower, upper, opt.method, dynamic, xreg=NULL,
                  lambda=NULL,   ## parameter of Box-Cox transformation,
                  nSample=10000,   ## used to compute bootstrap prediction intervals,
                  s = NULL  ## deprecated argument
                  )
  {

	if(!is.ts(y)){ stop("ERROR in twoTL function: y must be an object of time series class."); }
	if(!is.numeric(h)){	stop("ERROR in twoTL function: h must be a positive integer number.");}
	if( any(par_ini < lower) ||  any(par_ini > upper) ){stop("ERROR in twoTL function: par_ini out of range.");}

  # Conversion of deprecated argument to new arguments.
  if(!is.null(s)){
    if(is.logical(s)){
      s_test = s
    }else{
      if (s == "additive"){
        s_type = "additive"
      }
    }
  }

	n = length(y)
	fq = frequency(y)
	time_y = time(y)

	if(!is.null(lambda)){
	  if(lambda == "auto"){
	    lambda = BoxCox.lambda(y, lower=0, upper=1)
	  }
	  y = BoxCox(y, lambda)
	}

	if(!is.null(xreg)){

		if(nrow(xreg) != n+h)
			stop("ERROR: xreg must be a matrix with nrow(xreg) == length(y)+h");

		nreg <- ncol(xreg)

		if(length(par_ini)==3)
			par_ini <- c(par_ini,rep(0, nreg))
		if(length(lower)==3)
			lower <- c(lower,rep(-1e+100, nreg))
		if(length(upper)==3)
			upper <- c(upper,rep(1e+100, nreg))

		if(length(par_ini)!=3+nreg)
			stop("ERROR: error in the length of vector par_ini");
		if(length(lower)!=3+nreg)
			stop("ERROR: error in the length of vector lower");
		if(length(upper)!=3+nreg)
			stop("ERROR: error in the length of vector upper");
	}

	#### seasonal test and decomposition ###

	run_s_decomp = seasonal_test(y, s_test)
  if( fq < 3 ){
    s_type="None";s_type="None";
  }

	if( run_s_decomp ){

	  s_type = match.arg(arg=s_type, choices=c("additive","multiplicative","stl"))

	  if( s_type == "multiplicative" ){
	    y_decomp = decompose(y, type = "multiplicative")$seasonal
	    y = y/y_decomp
	  }

	  if( s_type == "additive" ){
	    y_decomp = decompose(y, type = "additive")$seasonal
	    y = y- y_decomp
	  }

	  if( s_type == "stl" ){
	    y_decomp = mstl(y)[,3]
	    y = y - y_decomp
	  }

	}
	########################################

	tnnTest.pvalue = terasvirta.test(y)$p.value

	mu = ell = A = B = meanY = numeric(n+h)

	Bn = 6*( 2*mean( (1:n)*y ) - (1+n)*mean(y) )/(n^2 -1)
	An = mean(y) - (n+1)*Bn/2

	new_y = as.numeric(y)

	# state space model
	SSM = function(par, computeForec=FALSE){
		ell0 = par[1]
		alpha = par[2]
		theta = par[3]

		if(!is.null(xreg)){
			reg = par[-(1:3)]
			y_reg  = as.numeric(xreg %*% reg)
			new_y = as.numeric(y) - y_reg[1:n]
		}

		ell[1] = alpha*new_y[1] + (1-alpha)*ell0
		meanY[1] = new_y[1]
		if(dynamic){
			A[1] = new_y[1]
			B[1] = 0
			mu[1] = new_y[1]
		}else{
			A[1] = An
			B[1] = Bn
			mu[1] = ell0 + (1-1/theta)*( An + Bn)
		}


		limit = n
		if(computeForec){
			limit = n+h
			new_y = c(new_y, rep(NA,h))
		}

		for(i in 1:(limit-1)){

			mu[i+1] = ell[i] + (1-1/theta)*( A[i]*((1-alpha)^i) + B[i]*(1-(1-alpha)^(i+1))/alpha )
			if(i >= n){
				new_y[i+1] = mu[i+1]
			}
			ell[i+1] = alpha*new_y[i+1] + (1-alpha)*ell[i]
			meanY[i+1] = (i*meanY[i] + new_y[i+1])/(i+1)
			if(dynamic){
				B[i+1] = ((i-1)*B[i] +6*(new_y[i+1] -meanY[i])/(i+1) )/(i+2)
				A[i+1] = meanY[i+1] - B[i+1]*(i+2)/2
			}else{
				A[i+1] = An
				B[i+1] = Bn
			}
		}

		if(!is.null(xreg)){
			if(computeForec)
				mu = mu + y_reg
			else
				mu[1:n] = mu[1:n] + y_reg[1:n]
		}

		return( list(mu=mu, ell=ell, A=A, B=B, meanY=meanY) )
	}

	cte = mean(abs(y)) ## constante

	# sun of squared error
	sse = function(par){
		if( any(par < lower) ||  any(par > upper) ){return(1e+300);}
		mu = SSM(par)$mu
		errors = (y[1:n] - mu[1:n]) / cte
		if(dynamic){
			return( sum(errors[3:n]^2) )
		}else{
			return( sum(errors^2) )
		}
	}

	if(estimation){
		if(opt.method == 'Nelder-Mead')
			opt = optim( par=par_ini, fn=sse, method="Nelder-Mead" )
		if(opt.method == 'L-BFGS-B')
			opt = optim( par=par_ini, fn=sse, method="L-BFGS-B", lower=lower, upper=upper)
		if(opt.method == 'SANN')
			opt = optim( par=par_ini, fn=sse, method="SANN")

		par = opt$par
	}else{
		par = par_ini
	}

	out.SSM = SSM(par, computeForec=TRUE)
	mu = out.SSM$mu
	Y_fcast = ts(mu[(n+1):(n+h)], start = end(y) + c(0, 1), frequency = frequency(y))
	Y_fitted = mu[1:n]
	Y_residuals = y - Y_fitted

	shapTest.pvalue = shapiro.test(Y_residuals[tail(3:n,4999)])$p.value

	matForec.sample = NULL ## initialize

	if(!is.null(level)){
		#nSample=200
		level = sort(level)
		alpha = par[2]
		theta = par[3]
		A = out.SSM$A[n]
		B = out.SSM$B[n]
		ell = out.SSM$ell[n]
		meanY = mean(y)
		sd.error = sd(Y_residuals[3:n])
		matForec.sample = matrix(NA,nrow=nSample,ncol=h)
		colnames(matForec.sample) = paste("h=",1:h,sep="")
		#matForec.sample[,1] = Y_fcast[1] + rnorm(nSample, 0, sd.error)
		for( i in n:(n+h-1) ){
			matForec.sample[,i+1-n] = ell + (1-1/theta)*( A*((1-alpha)^i) + B*(1-(1-alpha)^(i+1))/alpha ) + rnorm(nSample, 0, sd.error)
			ell = alpha*matForec.sample[,i+1-n] + (1-alpha)*ell
			meanY = (i*meanY + matForec.sample[,i+1-n])/(i+1)
			B = ( (i-1)*B +6*(matForec.sample[,i+1-n] -meanY)/(i+1) )/(i+2)
			A = meanY - B*(i+2)/2
		}

		# nn = length(level)
		# qq = (1-0.01*level)/2
		# probs = numeric(2*nn)
		# probs[2*(1:nn)-1] = qq
		# probs[2*(1:nn)] = 1-qq
		#
		# quantiles = t( apply(X=matForec.sample, MARGIN=2, FUN=quantile, probs=probs) )

		## included in version 2.7.3, it's used to compute bagging forecasts
		matForec.sample = t( matForec.sample )
		quantiles = bootstrap_quantiles( bs_series=matForec.sample, level=level )
		quantiles = ts( quantiles, start = end(y) + c(0, 1), frequency = frequency(y))
		#colnames(quantiles) = unlist( lapply(X=1:length(level), FUN=function(X) paste(c('Lo','Hi'), level[X]) ) )
	  matForec.sample = ts( matForec.sample , start = end(y) + c(0, 1), frequency = frequency(y) )
	  colnames( matForec.sample ) = paste0( "sample", 1:ncol(matForec.sample) )
		##

		if(!is.null(xreg)){
			reg = par[-(1:3)]
			y_reg  = as.numeric(xreg[-(1:n),] %*% reg)
			quantiles = quantiles + y_reg

			matForec.sample = matForec.sample + y_reg;
		}

	}

	if(run_s_decomp){
		if(s_type == 'multiplicative'){
			y = y * y_decomp
			Y_fitted = y_decomp * Y_fitted
			s_forec = snaive(y_decomp, h = h)$mean
			Y_fcast =  s_forec * Y_fcast
			if(!is.null(level)){
				# for(i in 1:ncol(quantiles)){
				# 	quantiles[,i] = quantiles[,i] * s_forec
				# }
			  quantiles = quantiles * replicate( ncol(quantiles), s_forec )

			  matForec.sample = matForec.sample * replicate( ncol(matForec.sample), s_forec )
			}

		}else{
			y = y + y_decomp
			Y_fitted = y_decomp + Y_fitted
			s_forec = snaive(y_decomp, h = h)$mean
			Y_fcast =  s_forec + Y_fcast
			if(!is.null(level)){
				# for(i in 1:ncol(quantiles)){
				# 	quantiles[,i] = quantiles[,i] + s_forec
				# }
			  quantiles = quantiles + replicate( ncol(quantiles), s_forec )

			  matForec.sample = matForec.sample + replicate( ncol(matForec.sample), s_forec )
			}

		}

		Y_residuals = y - Y_fitted
	}

	if(!is.null(lambda)){
	  y = InvBoxCox(y, lambda)
	  Y_fcast = InvBoxCox(Y_fcast, lambda)
	  if(!is.null(level)){
	    quantiles = InvBoxCox(quantiles, lambda)
	    matForec.sample =  InvBoxCox(matForec.sample, lambda);
	  }
	}

	out = list()
	out$method = "Two Theta Lines Model"
	out$y = y
	out$s_type = s_type
	out$s_test = s_test
	out$opt.method = ifelse(estimation, opt.method, 'none')
	out$par = matrix(par, ncol=1)
	out$lambda = lambda
	if(is.null(xreg)){
		rownames(out$par) = c('ell0','alpha','theta')
	}else{
		rownames(out$par) =c('ell0','alpha','theta', paste0("p",1:ncol(xreg)))
	}
	colnames(out$par) = 'MLE'
	omega = 1 - 1/par[3]
	out$weights = matrix( c(omega, 1-omega), ncol=1, nrow=2)
	rownames(out$weights) = c('omega_1', 'omega_2')
	colnames(out$weights) = 'Estimative'
	out$fitted = ts(Y_fitted, start = start(y), frequency = frequency(y))
	out$residuals = ts(Y_residuals, start = start(y), frequency = frequency(y))
	out$mean = Y_fcast
	out$level = level
	if(!is.null(level)){
		nn = length(level)
		out$lower = quantiles[, 2*(1:nn)-1, drop=F]
		out$upper = quantiles[, 2*(1:nn), drop=F]
	}else{
		out$lower = out$upper = NULL
	}
  out$matForec.sample = matForec.sample
	out$tests = matrix(c(tnnTest.pvalue,shapTest.pvalue),nrow=2)
	rownames(out$tests) = c('tnn-test','shapiro-test')
	colnames(out$tests) = c('p.value')

	return(	structure(out,class="thetaModel") )
}


######### Theta Models #########################################################
## models of
## Fiorucci J.A., Pellegrini T.R., Louzada F., Petropoulos F.,  Koehler, A.
## (2016). Models for optimising the theta method and their relationship to
## state space models, International Journal of Forecasting, 32 (4), 1151–1161,
## <doi:10.1016/j.ijforecast.2016.02.005>
dotm <- function(y, h=5, level=c(80,90,95),
                 s_type="multiplicative",
                 s_test="default",
                 lambda=NULL, par_ini=c(y[1]/2, 0.5, 2), estimation=TRUE,
                 lower=c(-1e+10, 0.1, 1.0), upper=c(1e+10, 0.99, 1e+10),
                 opt.method="Nelder-Mead", xreg=NULL, s=NULL ){

  out =  twoTL( y=y, h=h, level=level,
                s_type=s_type, s_test=s_test, par_ini=par_ini,
                estimation=estimation, lower=lower, upper=upper, opt.method=opt.method,
                dynamic=TRUE, xreg=xreg, lambda=lambda, s=s)

  out$method = "Dynamic Optimised Theta Model"

  return(out)
}

dstm <- function(y, h=5, level=c(80,90,95),
                 s_type="multiplicative", s_test="default",
                 lambda=NULL, par_ini=c(y[1]/2, 0.5),estimation=TRUE,
                 lower=c(-1e+10, 0.1), upper=c(1e+10, 0.99),
                 opt.method="Nelder-Mead", xreg=NULL,
                 s=NULL){

  out =  twoTL( y=y, h=h, level=level,
                s_type=s_type, s_test=s_test, par_ini=c(par_ini,2.0),
                estimation=estimation, lower=c(lower, 1.99999), upper=c(upper, 2.00001),
                opt.method=opt.method, dynamic=TRUE, xreg=xreg, lambda=lambda, s=s)

  out$method = "Dynamic Standard Theta Model"
  out$par = as.matrix(out$par[c('ell0','alpha'),])
  colnames(out$par) = 'MLE'

  return(out)
}


otm <- function(y, h=5, level=c(80,90,95),
                s_type="multiplicative", s_test="default",
                lambda=NULL, par_ini=c(y[1]/2, 0.5, 2.0), estimation=TRUE,
                lower=c(-1e+10, 0.1, 1.0), upper=c(1e+10, 0.99, 1e+10),
                opt.method="Nelder-Mead", xreg=NULL, s=NULL){

  out = twoTL( y=y, h=h, level=level,
               s_type=s_type, s_test=s_test,
               par_ini=par_ini, estimation=estimation, lower=lower,
               upper=upper, opt.method=opt.method, dynamic=FALSE, xreg=xreg,
               lambda=lambda, s=s)

  out$method = "Optimised Theta Model"

  return(out)
}

stm <- function(y, h=5, level=c(80,90,95),
                s_type="multiplicative", s_test="default",
                lambda=NULL, par_ini=c(y[1]/2, 0.5), estimation=TRUE,
                lower=c(-1e+10, 0.1), upper=c(1e+10, 0.99),
                opt.method="Nelder-Mead", xreg=NULL, s=NULL){

  out = twoTL( y=y, h=h, level=level,
               s_type=s_type, s_test=s_test,
               par_ini=c(par_ini,2.0), estimation=estimation,
               lower=c(lower,1.99999), upper=c(upper,2.00001),
               opt.method=opt.method, dynamic=FALSE, xreg=xreg, lambda=lambda,
               s=s)

  out$method = "Standard Theta Model"
  out$par = as.matrix(out$par[c('ell0','alpha'),])
  colnames(out$par) = 'MLE'

  return(out)
}
################################################################################



############ Bagged Models #####################################################
bagged_twoTL <- function(y, h, level,
                        num_bootstrap = 1,   # number of bootstrap replication
                        bs_bootstrap = NULL, # bootstrap block size
                        s_type, ## s_type = c("additive","multiplicative","stl")
                        s_test, ## s_test = c("default","unit_root",TRUE, FALSE)
                        par_ini, estimation, lower, upper, opt.method, dynamic, xreg=NULL,
                        lambda=NULL  ## parameter of Box-Cox transformation
                        )
{

  nSample = 10000
  y_bagged = list(y)
  if(num_bootstrap > 1 ){
    y_bagged = bld.mbb.bootstrap(y, num=num_bootstrap)
    nSample = max( nSample %/% num_bootstrap, 1)
  }

  if(num_bootstrap>1 && is.null(level)){level=c(80,90,95);} ## important for running bagging

  y_i = NULL
  models = foreach(y_i = y_bagged) %do% {

      fit = twoTL(y=y_i, h=h, level=level, s_type=s_type, s_test=s_test,
              par_ini=par_ini, estimation=estimation, lower=lower, upper=upper,
              opt.method=opt.method, dynamic=dynamic, xreg=xreg, lambda=lambda,
              nSample=nSample)

      fit
  }

  out = models[[1]]
  out$num_bootstrap = num_bootstrap

  if( num_bootstrap == 1 ){
    out$matForec.sample = NULL
    return(out);
  }

  # bs_means = foreach(fit = models, .combine=cbind) %do% { fit$mean; }
  # colnames(bs_means) = paste0("m", 1:ncol(bs_means) )
  # bs_means_mean = ts(rowMeans(bs_means), start=start(bs_means), frequency = frequency(y))
  # bs_means_median = apply(X=bs_means, MARGIN=1, FUN=quantile,  probs=0.5)
  # bs_means_median = ts(bs_means_median, start=start(bs_means), frequency = frequency(y))
  # out$bs_means_mean = bs_means_mean
  # out$bs_means_median = bs_means_median

  bs_strapolations = foreach(fit = models, .combine=cbind) %do% { fit$matForec.sample; }
  colnames(bs_strapolations) = paste0("m", 1:ncol(bs_strapolations) )
  bs_st_mean = ts(rowMeans(bs_strapolations), start=start(out$mean), frequency = frequency(y))
  bs_st_median = apply(X=bs_strapolations, MARGIN=1, FUN=quantile,  probs=0.5)
  bs_st_median = ts(bs_st_median, start=start(out$mean), frequency = frequency(y))

  out$mean = bs_st_mean
  out$median =  bs_st_median

  quantiles = bootstrap_quantiles( bs_series=bs_strapolations, level=level )
  quantiles = ts( quantiles, start = end(y) + c(0, 1), frequency = frequency(y))
  nn = length(level)
  out$lower = quantiles[, 2*(1:nn)-1, drop=F]
  out$upper = quantiles[, 2*(1:nn), drop=F]

  out$matForec.sample = NULL
  out$tests = NULL

  return(out)
}




bagged_dotm <- function(y, h=5, level=c(80,90,95),
                 num_bootstrap = 100,   # number of bootstrap replication
                 bs_bootstrap = NULL, # bootstrap block size
                 s_type="multiplicative",
                 s_test="default",
                 lambda=NULL, par_ini=c(y[1]/2, 0.5, 2), estimation=TRUE,
                 lower=c(-1e+10, 0.1, 1.0), upper=c(1e+10, 0.99, 1e+10),
                 opt.method="Nelder-Mead", xreg=NULL ){

  out =  bagged_twoTL( y=y, h=h, level=level,
                       num_bootstrap = num_bootstrap, bs_bootstrap = bs_bootstrap,
                       s_type=s_type, s_test=s_test, par_ini=par_ini,
                       estimation=estimation, lower=lower, upper=upper, opt.method=opt.method,
                       dynamic=TRUE, xreg=xreg, lambda=lambda)

  out$method = "Dynamic Optimised Theta Model"
  if(out$num_bootstrap > 1){ out$method = paste("Bagged", out$method); }

  return(out)
}

bagged_dstm <- function(y, h=5, level=c(80,90,95),
                 num_bootstrap = 100,   # number of bootstrap replication
                 bs_bootstrap = NULL, # bootstrap block size
                 s_type="multiplicative", s_test="default",
                 lambda=NULL, par_ini=c(y[1]/2, 0.5),estimation=TRUE,
                 lower=c(-1e+10, 0.1), upper=c(1e+10, 0.99),
                 opt.method="Nelder-Mead", xreg=NULL){

  out =  bagged_twoTL( y=y, h=h, level=level,
                       num_bootstrap = num_bootstrap, bs_bootstrap = bs_bootstrap,
                       s_type=s_type, s_test=s_test, par_ini=c(par_ini,2.0),
                       estimation=estimation, lower=c(lower, 1.99999), upper=c(upper, 2.00001),
                       opt.method=opt.method, dynamic=TRUE, xreg=xreg, lambda=lambda)

  out$method = "Dynamic Standard Theta Model"
  if(out$num_bootstrap > 1){ out$method = paste("Bagged", out$method); }
  out$par = as.matrix(out$par[c('ell0','alpha'),])
  colnames(out$par) = 'MLE'

  return(out)
}


bagged_otm <- function(y, h=5, level=c(80,90,95),
                num_bootstrap = 100,   # number of bootstrap replication
                bs_bootstrap = NULL, # bootstrap block size
                s_type="multiplicative", s_test="default",
                lambda=NULL, par_ini=c(y[1]/2, 0.5, 2.0), estimation=TRUE,
                lower=c(-1e+10, 0.1, 1.0), upper=c(1e+10, 0.99, 1e+10),
                opt.method="Nelder-Mead", xreg=NULL){

  out = bagged_twoTL( y=y, h=h, level=level,
                      num_bootstrap = num_bootstrap, bs_bootstrap = bs_bootstrap,
                      s_type=s_type, s_test=s_test,
                      par_ini=par_ini, estimation=estimation, lower=lower,
                      upper=upper, opt.method=opt.method, dynamic=FALSE, xreg=xreg,
                      lambda=lambda)

  out$method = "Optimised Theta Model"
  if(out$num_bootstrap > 1){ out$method = paste("Bagged", out$method); }

  return(out)
}

bagged_stm <- function(y, h=5, level=c(80,90,95),
                num_bootstrap = 100,   # number of bootstrap replication
                bs_bootstrap = NULL, # bootstrap block size
                s_type="multiplicative", s_test="default",
                lambda=NULL, par_ini=c(y[1]/2, 0.5), estimation=TRUE,
                lower=c(-1e+10, 0.1), upper=c(1e+10, 0.99),
                opt.method="Nelder-Mead", xreg=NULL){

  out = bagged_twoTL( y=y, h=h, level=level,
                      num_bootstrap = num_bootstrap, bs_bootstrap = bs_bootstrap,
                      s_type=s_type, s_test=s_test,
                      par_ini=c(par_ini,2.0), estimation=estimation,
                      lower=c(lower,1.99999), upper=c(upper,2.00001),
                      opt.method=opt.method, dynamic=FALSE, xreg=xreg, lambda=lambda)

  out$method = "Standard Theta Model"
  if(out$num_bootstrap > 1){ out$method = paste("Bagged", out$method); }
  out$par = as.matrix(out$par[c('ell0','alpha'),])
  colnames(out$par) = 'MLE'

  return(out)
}
################################################################################




###################  SES  ######################################################
expSmoot <- function(y, h=5, ell0=NULL, alpha=NULL, lower = c(-1e+10, 0.1),
                     upper = c(1e+10, 0.99)){
	n = length(y)

	mu = error = ell = numeric(n+h)

	#### state space model
	SSM <- function(ell0, alpha){

		mu[1] = ell0
		error[1] = y[1] - mu[1]
		ell[1] = ell0 + alpha*error[1]

		for(i in 2:(n+h)){
			mu[i] = ell[i-1]
			error[i] = ifelse(i<=n, y[i] - mu[i], 0.0)
			ell[i] = ell[i-1] + alpha*error[i]
		}

		return( list(mean = mu[(n+1):(n+h)], fitted = mu[1:n], error = error[1:n],
		             ell0=ell0, alpha=alpha)  )
	}

	### sum of square error
	cte = mean(abs(y)) ## just to evoid optim errors of non finete values
	sse <- function(par){
		if( any(par < lower) ||  any(par > upper) ){return(Inf);}
		ell0=par[1]; alpha=par[2];
		error = SSM( ell0, alpha )$error / cte
		return( sum(  error^2  ) )
	}

	par = c(ell0, alpha)

	if( is.null(ell0) || is.null(alpha) ){

		alpha_ini = 0.5
		maxn = min(10, n)
		fit1 = lsfit(1:maxn, y[1:maxn])
		ell0_ini = fit1$coef[1]

		par_ini = c( ell0_ini , alpha_ini )


		if(!is.null(ell0)){  par_ini[1] = ell0;  upper[1] = ell0 +0.00001; lower[1] = ell0 -0.00001; }
		if(!is.null(alpha)){ par_ini[2] = alpha; upper[2] = alpha+0.00001; lower[2] = alpha-0.00001; }

		#opt = optim( par=par_ini, fn=sse, method="L-BFGS-B", lower=lower, upper=upper )
		opt = optim( par=par_ini, fn=sse, method="Nelder-Mead" )
		par = opt$par
	}

	out = SSM(ell0=par[1], alpha=par[2])

	Y_fcast = ts(out$mean, start = end(y) + c(0, 1), frequency = frequency(y))
	Y_fitted = out$fitted


	Y_residuals = y - Y_fitted

	fit = list()
	fit$par = matrix(par, ncol=1, nrow=2)
	rownames(fit$par) = c('ell0','alpha')
	fit$mean = Y_fcast
	fit$fitted = ts(Y_fitted, start = start(y), frequency = frequency(y))
	fit$residuals = Y_residuals

	return(fit)

}
################################################################################



####### OTM as implementade in Fioruci et al (2015)#############################
## Fioruci J.A., Pellegrini T.R., Louzada F., Petropoulos F. (2015). The Optimised
# Theta Method. arXiv preprint, arXiv:1503.03529.
otm.arxiv <- function( y, h=5, s=NULL, theta=NULL, tLineExtrap=expSmoot, g="sAPE",
		approach="c",
		n1=NULL, m=NULL, H=NULL, p=NULL,
		thetaList=seq(from=1,to=5,by=0.5),
		mc.cores=1,...){

	if(!is.ts(y)){ stop("ERROR in otm.arxiv function: y must be a object of time series class.") }
	if(!is.numeric(h)){	stop("ERROR in otm.arxiv function: h must be a positive integer number.")	}
	if(!is.null(theta) && !is.numeric(theta)){ stop("ERROR in otm.arxiv function: theta must be a numeric value higher or equal to 1 or NULL.")}
	if(is.numeric(theta) && theta < 1){	stop("ERROR in otm.arxiv function: theta must be a numeric value higher or equal to 1 or NULL.")}
	if( !is.null(approach) && !is.null(c(n1,m,H,p)) ){stop("ERROR in otm.arxiv function: do approach=NULL if you want to set a new approach for groe parameters.")}
	n = length(y)
	if(is.null(theta) && !is.null(approach)){
		if( any(approach == c('a','b','c','d','e','f','g','h')) ){
			if(approach == 'a'){n1=n-h; m=h; H=h;p=1;}
			if(approach == 'b'){n1=n-h; m=floor(h/2); H=h; p=2;}
			if(approach == 'c'){n1=n-h; m=floor(h/3); H=h; p=3;}
			if(approach == 'd'){n1=n-h; m=1; H=h; p=h;}
			if(approach == 'e'){n1=n-2*h; m=h; H=h; p=2;}
			if(approach == 'f'){n1=n-2*h; m=floor(h/2); H=h; p=4;}
			if(approach == 'g'){n1=n-2*h; m=floor(h/3); H=h; p=6;}
			if(approach == 'h'){n1=n-2*h; m=1; H=h; p=h;}
			if(n1 < 4){n1=4;}
		}else{
			stop("ERROR in otm function: the argument 'approach' must lie between 'a' and 'h')")
		}
	}
	if(is.null(theta) && n1 < 4) stop("ERROR in otm function: n1 < 4)")
	if(is.null(theta) && p > 1+floor((length(y)-n1)/m)){ stop("ERROR in otm function: p > 1+floor((length(y)-n1)/m)") }

	x=y
	fq = frequency(y)
	time_y = time(y)

	if( is.null(s) && any(fq == c(4,12)) ){
		xacf = acf(y,plot=FALSE)$acf[-1,1,1]
		clim = 1.64/sqrt(length(y))*sqrt(cumsum(c(1,2*xacf^2)))
		s = abs(xacf[fq]) > clim[fq]
	}else{
		if(is.null(s)){
			s = FALSE
		}
	}

	if(s){
        y_decomp = decompose(y, type = "multiplicative")$seasonal
        y = y/y_decomp
    }

	if(is.null(theta)){

		lossFunctionList = as.numeric(
			mclapply( X=thetaList, FUN=function(theta) groe( y=y, forecFunction=otm.arxiv, g=g,
														n1=n1, m=m, H=H, p=p, theta=theta, tLineExtrap=tLineExtrap,...),
						mc.cores = mc.cores )
		)

		aux = which.min(lossFunctionList)
		theta = thetaList[aux]
	}

	time_forec = n+(1:h) #time_y[n] + (1:h)/fq
	omega = 1 - 1/theta

	## linear method
	tt = 1:n
	l = lm(y ~ tt) ##lm(y ~ time_y)
	l$mean = l$coeff[1] + l$coeff[2]*time_forec ##l$coeff[1] + l$coeff[2]*time_forec

	## other extrapolation method
	Z = l$fitted.values  +   theta * l$residuals
    X = tLineExtrap(Z, h=h, ...)

	Y_fitted = as.numeric( omega*l$fitted.values + (1-omega)*X$fitted )
	Y_fitted = ts(Y_fitted, start = start(y), frequency = frequency(y))
    Y_fcast = as.numeric( omega*l$mean + (1-omega)*X$mean )
	Y_fcast = ts(Y_fcast, start = end(y)+c(0,1), frequency = frequency(y))
    Y_residuals = y - Y_fitted

	if(s){
		y = y*y_decomp
        Y_fitted = y_decomp * Y_fitted
        Y_fcast = snaive(y_decomp, h = h)$mean * Y_fcast
        Y_residuals = y - Y_fitted
    }

    out = list()
	out$y = x
	out$s = s
    out$mean = Y_fcast
    out$fitted = Y_fitted
    out$residuals = Y_residuals
	out$theta = theta
	if(!is.null(X$model$par)){
		out$tLineExtrap_par = X$model$par
	}
	out$weights = c(omega, 1-omega)

	return(structure(out,class="otm"))
}


stheta <- function (y, h=5, s_type="multiplicative", s_test="default", s=NULL)
{
	if(!is.ts(y)){ stop("ERROR in stheta function: y must be a object of time series class."); }
	if(!is.numeric(h)){	stop("ERROR in stheta function: h must be a positive integer number.");}

  # Conversion of deprecated argument to new arguments.
  if(!is.null(s)){
    if(is.logical(s)){
      s_test = s
    }else{
      if (s == "additive"){
        s_type = "additive"
      }
    }
  }

	n = length(y)
	fq = frequency(y)
	time_y = time(y)
	time_forec = time_y[n] + (1:h)/fq
	#s_type = 'multiplicative'
	#s_test = "default"

	x=y

	#### seasonal test and decomposition ###
	run_s_decomp = seasonal_test(y, s_test)

	if( fq < 3 ){
	  s_type="None";s_type="None";
	}

	if( run_s_decomp ){

	  s_type = match.arg(arg=s_type, choices=c("additive","multiplicative","stl"))

	  if( s_type == "multiplicative" ){
	    y_decomp = decompose(y, type = "multiplicative")$seasonal
	    y = y/y_decomp
	  }

	  if( s_type == "additive" ){
	    y_decomp = decompose(y, type = "additive")$seasonal
	    y = y- y_decomp
	  }

	  if( s_type == "stl" ){
	    y_decomp = mstl(y)[,3]
	    y = y - y_decomp
	  }

	}
	########################################

	l = lm(y ~ time_y)
	l$mean = l$coeff[1] + l$coeff[2] * time_forec
	Z <- l$fitted.values + 2 * l$residuals
	X <- expSmoot(y=Z, h=h)

	out = stm(y=x, h=h, level=NULL, s_type=s_type, s_test=s_test,
	          par_ini=c(X$par[1]/2, X$par[2]), estimation=FALSE)

	out$method = 'Standard Theta Method (STheta)'
	out$opt.method = 'Nelder-Mead'
	out$par = as.matrix(c(out$par[1]*2, out$par[2]))
	rownames(out$par) = c('ell0^*','alpha')
	colnames(out$par) = 'MLE'
	return(out)
}

################################################################################

#' @export
print.thetaModel <- function(x,...){

	cat(paste("Forecast method:", x$method, "\n\n"))

  cat(paste("Seasonal decomposition type:", x$s_type, "\n\n"))

  cat(paste("Seasonal test:", x$s_test, "\n\n"))

	cat(paste("Optimisation method:", x$opt.method,"\n\n") )

	cat(paste("Number of theta lines:", 2, "\n\n"))

	cat("Weights for theta lines:\n")
	print(round(x$weights,2))

	cat("\nEstimative of parameters:\n")
	print(round(x$par,2))

	if( !is.null(x$lambda) ){
	  cat("\nBoxCox transformation: lambda =", round(x$lambda,2), "\n")
	}

	if(!is.null(x$lower)&&!is.null(x$upper)){
		cat("\nForecasting points and prediction intervals\n")
		mm = cbind(x$mean,x$lower,x$upper)
		colnames(mm) = c('Mean', colnames(x$lower), colnames(x$upper) )
		level = x$level
		nn = length(level)
		sortNames = c('Mean', unlist( lapply(X=1:nn, FUN=function(X) paste(c('Lo','Hi'), level[X]) ) ) )
		mm = mm[,sortNames,drop=F]
 	}else{
		cat("\nForecasting points\n")
		mm = x$mean
	}

	print( round(mm, 4 ) )

	#if(x$tests[1,1] < 0.02){cat("\nWarning: According with the Teraesvirta Neural Network test with 98% of confidence, the unseasoned time series is not linearity in mean. This model may not be adequate.\n")}
	if(is.null(x$num_bootstrap)){
  	if(x$tests[2,1] < 0.03){
  	  cat("\nWarning: According with the Shapiro-Wilk test with 97% of confidence,
      the unseasoned residuals do not follow the Normal distribution.
      The prediction intervals may not be adequate.
      Consider using the bagged version of this model.\n")
  	}
	}

}

#' @export
summary.thetaModel <- function(object,...){

	out = list()

	out$method = object$method

	out$s_type =  object$s_type

	out$s_test =  object$s_test

	out$opt.method = object$opt.method

	out$par = object$par

	out$lambda = object$lambda

	if(!is.null(object$lower)&&!is.null(object$upper)){
		mm = cbind(object$mean,object$lower,object$upper)
		colnames(mm) = c('Mean', colnames(object$lower), colnames(object$upper) )
		level = object$level
		nn = length(level)
		sortNames = c('Mean', unlist( lapply(X=1:nn, FUN=function(X) paste(c('Lo','Hi'), level[X]) ) ) )
		mm = mm[,sortNames,drop=F]
 	}else{
		mm = object$mean
	}

	out$statistics = mm

	n = length(object$y)
	out$sigma = sd(object$residuals[3:n])

	np = length(object$par)
	logLikelihoood = -0.5*n*( log(out$sigma^2) + 1 + log(2*pi) )
	aic = -2*logLikelihoood + 2*np
	aicc = aic + 2*np*(np+1)/(n-np-1)
	bic = -2*logLikelihoood + log(n)*np

	ic = matrix(c(aic,aicc,bic), nrow=3, ncol=1)
	rownames(ic) = c('AIC','AICc','BIC')
	colnames(ic) = 'Estimative'

	out$informationCriterions = ic

	out$tests = object$tests

	if( !is.null(object$num_bootstrap))
	  out$num_bootstrap = object$num_bootstrap

	return(structure(out,class="summ"))
}

#' @export
print.summ <- function(x,...){
	cat(paste("Forecast method:", x$method, "\n\n"))

  cat(paste("Seasonal decomposition type:", x$s_type, "\n\n"))

  cat(paste("Seasonal test:", x$s_test, "\n\n"))

	cat(paste("Optimisation method:", x$opt.method,"\n\n"))

	cat("Estimative of parameters:\n")
	print(round(x$par,2))

	if( !is.null(x$lambda) ){
	  cat("\nBoxCox transformation: lambda =", round(x$lambda,2), "\n")
	}

	cat("\nForecasting points and prediction intervals\n")
	print(x$statistics)

	cat("\nInformation Criterions\n")
	print(x$informationCriterions)

	#if(x$tests[1,1] < 0.02){cat("\nWarning: According with the Teraesvirta Neural Network test with 98% of confidence, the unseasoned time series is not linearity in mean. This model may not be adequate.\n")}
	if( is.null( x$num_bootstrap) ) {
	  if(x$tests[2,1] < 0.03){
	    cat("\nWarning: According with the Shapiro-Wilk test with 97% of confidence,
      the unseasoned residuals do not follow the Normal distribution.
      The prediction intervals may not be adequate.
      Consider using the bagged version of this model.\n")
	  }
	}
}

#' @export
plot.thetaModel <- function(x, ylim=NULL, xlim=NULL, ylab=NULL, xlab=NULL, main=NULL,...){
	h = length(x$mean)
	time_y = time(x$y)
	time_forec = time(x$mean)

	if(is.null(ylim)){ ylim=c( min(c(x$y,x$fitted,x$mean, x$lower)), max(c(x$y,x$fitted,x$mean, x$upper)));}
	if(is.null(xlim)){ xlim=c(time_y[1],time_forec[h]);}
	if(is.null(ylab)){ ylab="";}
	if(is.null(xlab)){ xlab="";}
	if(is.null(main)){ main=x$method;}

	plot(x$y, xlim=xlim, ylim=ylim, ylab=ylab, xlab=xlab, main=main, ...)

	if(!is.null(x$lower)&&!is.null(x$upper)){
		nlev = length(x$level)
		for(i in nlev:1){
			yy = c(x$lower[1,i],x$upper[1:h,i],x$lower[h:1,i])
			xx = c(time_forec[1], time_forec[1:h], time_forec[h:1])
			polygon(y=yy, x=xx, col=paste('grey', 40+(i-1)*10,sep=''), border=FALSE)
			#points(x$lower[,i], type = "l", col="red")
			#points(x$upper[,i], type = "l", col="red")
		}

		legend("topleft", legend=c('Forecasting', paste(x$level,'% Predict. Interv.', sep='')), lty=rep(1,nlev+1),
			lwd=c(3,rep(10,nlev)), col=c('blue',paste('grey', 40+((1:nlev) -1)*10,sep='') ), bty = "n" )
	}

	points(x$mean, type = "l", col="blue", lwd=3)
}


