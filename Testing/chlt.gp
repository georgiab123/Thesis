new;
library pgraph;
graphset;
format /rdt 8,3;


load path = /Users/georgiabradley/Documents/Thesis/MAKI_TEST/real;
load y[] = jkm_real.csv;

rep_wb = 400;

n = rows(y);

k_max = floor(12*((n/100)^(1/4)));

trm = 0.15;
g_brk_MZ = 6;
g_brk_ADF = 3;

cv_MZa_lim = -16.62;
cv_MSB_lim = 0.171;
cv_MZt_lim = -2.85;
cv_ers_lim = -2.85;

MZa_cv_vals_lim = -23.06|-23.49|-23.83|-23.95|-23.98|-23.95|-23.90|-23.77|-23.52|-23.25|-22.90|-22.46|-21.83|-21.16|-20.24;
MSB_cv_vals_lim = 0.146|0.144|0.143|0.143|0.143|0.143|0.143|0.144|0.145|0.146|0.147|0.148|0.151|0.153|0.157;
MZt_cv_vals_lim = -3.37|-3.40|-3.42|-3.43|-3.43|-3.44|-3.44|-3.42|-3.41|-3.39|-3.37|-3.34|-3.29|-3.24|-3.17;
ADF_cv_vals_lim = MZt_cv_vals_lim;

cbar_vals = -17.6|-17.8|-18.2|-18.4|-18.6|-18.4|-18.4|-18.2|-18.0|-17.6|-17.4|-17.0|-16.6|-16.0|-15.2;

tau_cbar_MZa_cv_lim = seqa(0.15,0.05,15)~cbar_vals~MZa_cv_vals_lim;
tau_cbar_MSB_cv_lim = seqa(0.15,0.05,15)~cbar_vals~MSB_cv_vals_lim;
tau_cbar_MZt_cv_lim = seqa(0.15,0.05,15)~cbar_vals~MZt_cv_vals_lim;
tau_cbar_ADF_cv_lim = seqa(0.15,0.05,15)~cbar_vals~ADF_cv_vals_lim;

	const = ones(n,1);
	tr = seqa(1,1,n);
	tb_l = floor(trm*n);
	tb_u = floor((1-trm)*n);

			dy = trimr(y-lag1(y),1,0);

			/* Break date estimates */

			ssr_dy = zeros(tb_u-tb_l+1,1);

			tb = tb_l;
			do while tb <= tb_u;

				b1 = (y[tb]-y[1])/(tb-1);
				b2 = ((y[n]-y[tb])/(n-tb))-b1;
				ssr_dy[tb-tb_l+1] = ((n-1)*b1^2)+((n-tb)*b2^2)-(2*b1*(y[n]-y[1]))-(2*b2*(y[n]-y[tb]))+(2*b1*b2*(n-tb));
				/* note dy'dy taken out of ssr_dy as common to all */

			tb = tb + 1;
			endo;

			tb_dy = minindc(ssr_dy)+tb_l-1;

			tau_dy = tb_dy/n;
			du_tb_dy = zeros(tb_dy,1)|ones(n-tb_dy,1);
			dt_tb_dy = zeros(tb_dy,1)|seqa(1,1,n-tb_dy);

			z = cumsumc(y);
			x = tr~cumsumc(tr)~cumsumc(dt_tb_dy);
			b = z/x;
			ru = z-(x*b);
			x = tr~cumsumc(tr);
			b = z/x;
			rr = z-(x*b);
			W_stat_dy = ((rr'rr)/(ru'ru))-1;

			lam_MZ_brk_tau_dy = exp(-g_brk_MZ*W_stat_dy/sqrt(n));
			tau_lam_MZ = (1-lam_MZ_brk_tau_dy)*tau_dy;

			lam_ADF_brk_tau_dy = exp(-g_brk_ADF*W_stat_dy/sqrt(n));
			tau_lam_ADF = (1-lam_ADF_brk_tau_dy)*tau_dy;

			/* Unit root tests */

			r_GLS_t = GLS_t(y);
			x_OLS_t = const~tr;
			b_OLS_t = y/x_OLS_t;
			r_OLS_t = y-(x_OLS_t*b_OLS_t);
			k_t = kMAIC(r_OLS_t,k_max);
			ers_t_k = DF(r_GLS_t,k_t);
			{MZa_k,MSB_k,MZt_k} = MZ(r_GLS_t,k_t);

			if tau_lam_MZ < trm;
				HHLT_MZa_kMAIC = MZa_k; HHLT_MZa_kMAIC_cv = cv_MZa_lim;
				HHLT_MSB_kMAIC = MSB_k; HHLT_MSB_kMAIC_cv = cv_MSB_lim;
				HHLT_MZt_kMAIC = MZt_k; HHLT_MZt_kMAIC_cv = cv_MZt_lim;
			else;
				if tau_lam_MZ == trm;
					cbar_tau_lam_MZ = tau_cbar_MZa_cv_lim[1,2];
					cv_MZa_tau_lam_MZ_lim = tau_cbar_MZa_cv_lim[1,3];
					cv_MSB_tau_lam_MZ_lim = tau_cbar_MSB_cv_lim[1,3];
					cv_MZt_tau_lam_MZ_lim = tau_cbar_MZt_cv_lim[1,3];
				elseif tau_lam_MZ == 1-trm;
					cbar_tau_lam_MZ = tau_cbar_MZa_cv_lim[15,2];
					cv_MZa_tau_lam_MZ_lim = tau_cbar_MZa_cv_lim[15,3];
					cv_MSB_tau_lam_MZ_lim = tau_cbar_MSB_cv_lim[15,3];
					cv_MZt_tau_lam_MZ_lim = tau_cbar_MZt_cv_lim[15,3];
				else;
					tau_near_index = minindc(abs(tau_lam_MZ-tau_cbar_MZa_cv_lim[.,1]));
					tau_near = tau_cbar_MZa_cv_lim[tau_near_index,1];
					if tau_lam_MZ > tau_near;
						tau_l = tau_near;
						tau_l_index = tau_near_index;
						tau_u = tau_near+0.05;
						tau_u_index = tau_near_index+1;
					else;
						tau_l = tau_near-0.05;
						tau_l_index = tau_near_index-1;
						tau_u = tau_near;
						tau_u_index = tau_near_index;
					endif;
					weight_l = 1-((tau_lam_MZ-tau_l)/0.05);
					weight_u = 1-((tau_u-tau_lam_MZ)/0.05);
					cbar_tau_lam_MZ = (weight_l*tau_cbar_MZa_cv_lim[tau_l_index,2])+(weight_u*tau_cbar_MZa_cv_lim[tau_u_index,2]);
					cv_MZa_tau_lam_MZ_lim = (weight_l*tau_cbar_MZa_cv_lim[tau_l_index,3])+(weight_u*tau_cbar_MZa_cv_lim[tau_u_index,3]);
					cv_MSB_tau_lam_MZ_lim = (weight_l*tau_cbar_MSB_cv_lim[tau_l_index,3])+(weight_u*tau_cbar_MSB_cv_lim[tau_u_index,3]);
					cv_MZt_tau_lam_MZ_lim = (weight_l*tau_cbar_MZt_cv_lim[tau_l_index,3])+(weight_u*tau_cbar_MZt_cv_lim[tau_u_index,3]);
				endif;
				r_GLS_bt = GLS_bt(y,tau_lam_MZ,cbar_tau_lam_MZ);
				tb_lam_MZ = floor(tau_lam_MZ*n);
				dtr = zeros(tb_lam_MZ,1)|seqa(1,1,n-tb_lam_MZ);
				x_OLS_bt = const~tr~dtr;
				b_OLS_bt = y/x_OLS_bt;
				r_OLS_bt = y-(x_OLS_bt*b_OLS_bt);
				k_bt = kMAIC(r_OLS_bt,k_max);
				{MZa_tau_lam_MZ_k,MSB_tau_lam_MZ_k,MZt_tau_lam_MZ_k} = MZ(r_GLS_bt,k_bt);
				HHLT_MZa_kMAIC = MZa_tau_lam_MZ_k; HHLT_MZa_kMAIC_cv = cv_MZa_tau_lam_MZ_lim;
				HHLT_MSB_kMAIC = MSB_tau_lam_MZ_k; HHLT_MSB_kMAIC_cv = cv_MSB_tau_lam_MZ_lim;
				HHLT_MZt_kMAIC = MZt_tau_lam_MZ_k; HHLT_MZt_kMAIC_cv = cv_MZt_tau_lam_MZ_lim;
			endif;

			if tau_lam_ADF < trm;
				HHLT_ADF_kMAIC = ers_t_k; HHLT_ADF_kMAIC_cv = cv_ers_lim;
			else;
				if tau_lam_ADF == trm;
					cbar_tau_lam_ADF = tau_cbar_ADF_cv_lim[1,2];
					cv_ADF_tau_lam_ADF_lim = tau_cbar_ADF_cv_lim[1,3];
				elseif tau_lam_ADF == 1-trm;
					cbar_tau_lam_ADF = tau_cbar_ADF_cv_lim[15,2];
					cv_ADF_tau_lam_ADF_lim = tau_cbar_ADF_cv_lim[15,3];
				else;
					tau_near_index = minindc(abs(tau_lam_ADF-tau_cbar_ADF_cv_lim[.,1]));
					tau_near = tau_cbar_ADF_cv_lim[tau_near_index,1];
					if tau_lam_ADF > tau_near;
						tau_l = tau_near;
						tau_l_index = tau_near_index;
						tau_u = tau_near+0.05;
						tau_u_index = tau_near_index+1;
					else;
						tau_l = tau_near-0.05;
						tau_l_index = tau_near_index-1;
						tau_u = tau_near;
						tau_u_index = tau_near_index;
					endif;
					weight_l = 1-((tau_lam_ADF-tau_l)/0.05);
					weight_u = 1-((tau_u-tau_lam_ADF)/0.05);
					cbar_tau_lam_ADF = (weight_l*tau_cbar_ADF_cv_lim[tau_l_index,2])+(weight_u*tau_cbar_ADF_cv_lim[tau_u_index,2]);
					cv_ADF_tau_lam_ADF_lim = (weight_l*tau_cbar_ADF_cv_lim[tau_l_index,3])+(weight_u*tau_cbar_ADF_cv_lim[tau_u_index,3]);
				endif;
				r_GLS_bt = GLS_bt(y,tau_lam_ADF,cbar_tau_lam_ADF);
				tb_lam_ADF = floor(tau_lam_ADF*n);
				dtr = zeros(tb_lam_ADF,1)|seqa(1,1,n-tb_lam_ADF);
				x_OLS_bt = const~tr~dtr;
				b_OLS_bt = y/x_OLS_bt;
				r_OLS_bt = y-(x_OLS_bt*b_OLS_bt);
				k_bt = kMAIC(r_OLS_bt,k_max);
				ers_tau_lam_ADF_0 = DF(r_GLS_bt,0);
				ers_tau_lam_ADF_k = DF(r_GLS_bt,k_bt);
				HHLT_ADF_kMAIC = ers_tau_lam_ADF_k; HHLT_ADF_kMAIC_cv = cv_ADF_tau_lam_ADF_lim;
			endif;

			/* Bootstrap */

			MZa_tau_lam_MZ_0_wb = zeros(rep_wb,1);
			MSB_tau_lam_MZ_0_wb = zeros(rep_wb,1);
			MZt_tau_lam_MZ_0_wb = zeros(rep_wb,1);
			ers_tau_lam_ADF_0_wb = zeros(rep_wb,1);

			x = trimr(const~du_tb_dy,1,0);
			b = dy/x;
			r = 0|(dy-(x*b));

			state = 290373;

			i_wb = 1;
			do while i_wb <= rep_wb;

				{z,state} = rndKMn(n,1,state);

				e_wb = r.*z;
				y_wb = cumsumc(e_wb);

				if tau_lam_MZ < trm;
					r_GLS_t_wb = GLS_t(y_wb);
					{MZa_tau_lam_MZ_0_wb[i_wb],MSB_tau_lam_MZ_0_wb[i_wb],MZt_tau_lam_MZ_0_wb[i_wb]} = MZ(r_GLS_t_wb,0);
				else;
					r_GLS_bt_wb = GLS_bt(y_wb,tau_lam_MZ,cbar_tau_lam_MZ);
					{MZa_tau_lam_MZ_0_wb[i_wb],MSB_tau_lam_MZ_0_wb[i_wb],MZt_tau_lam_MZ_0_wb[i_wb]} = MZ(r_GLS_bt_wb,0);
				endif;

				if tau_lam_ADF < trm;
					r_GLS_t_wb = GLS_t(y_wb);
					ers_tau_lam_ADF_0_wb[i_wb] = DF(r_GLS_t_wb,0);
				else;
					r_GLS_bt_wb = GLS_bt(y_wb,tau_lam_ADF,cbar_tau_lam_ADF);
					ers_tau_lam_ADF_0_wb[i_wb] = DF(r_GLS_bt_wb,0);
				endif;

			i_wb = i_wb + 1;
			endo;

			s_stat = sortc(MZa_tau_lam_MZ_0_wb,1);
			cv_MZa_0_wb = s_stat[int(0.05*rep_wb)];
			s_stat = sortc(MSB_tau_lam_MZ_0_wb,1);
			cv_MSB_0_wb = s_stat[int(0.05*rep_wb)];
			s_stat = sortc(MZt_tau_lam_MZ_0_wb,1);
			cv_MZt_0_wb = s_stat[int(0.05*rep_wb)];
			s_stat = sortc(ers_tau_lam_ADF_0_wb,1);
			cv_ADF_0_wb = s_stat[int(0.05*rep_wb)];

            s_stat = sortc(MZa_tau_lam_MZ_0_wb,1);
			cv_MZa_0_wb_1 = s_stat[int(0.1*rep_wb)];
			s_stat = sortc(MSB_tau_lam_MZ_0_wb,1);
			cv_MSB_0_wb_1 = s_stat[int(0.1*rep_wb)];
			s_stat = sortc(MZt_tau_lam_MZ_0_wb,1);
			cv_MZt_0_wb_1 = s_stat[int(0.1*rep_wb)];
			s_stat = sortc(ers_tau_lam_ADF_0_wb,1);
			cv_ADF_0_wb_1 = s_stat[int(0.1*rep_wb)];
            
print"MZ_alpha stat = " HHLT_MZa_kMAIC;
/*print"Asymptotic c.v. assuming constant volatility = " HHLT_MZa_kMAIC_cv;*/
print"Bootstrap c.v. = " cv_MZa_0_wb cv_MZa_0_wb_1;
print;
print"MSB stat = " HHLT_MSB_kMAIC;
/*print"Asymptotic c.v. assuming constant volatility = " HHLT_MSB_kMAIC_cv;*/
print"Bootstrap c.v. = " cv_MSB_0_wb cv_MSB_0_wb_1;
print;
print"MZ_t stat = " HHLT_MZt_kMAIC;
/*print"Asymptotic c.v. assuming constant volatility = " HHLT_MZt_kMAIC_cv;*/
print"Bootstrap c.v. = " cv_MZt_0_wb cv_MZt_0_wb_1;
print;
print"t(taubar) stat = " HHLT_ADF_kMAIC;
/*print"Asymptotic c.v. assuming constant volatility = " HHLT_ADF_kMAIC_cv;*/
print"Bootstrap c.v. = " cv_ADF_0_wb cv_ADF_0_wb_1;
print;


proc GLS_t(y);
    local n,y_ers,ct,z,beta,yd;
	n = rows(y);
    y_ers = y[1]|(y[2:n]-(1-(13.5/n))*y[1:n-1]);
	ct = ones(n,1)~seqa(1,1,n);
	z = ct[1,.]|(ct[2:n,.]-(1-(13.5/n))*ct[1:n-1,.]);
	beta = inv(z'z)*z'y_ers;
	yd = y-ct*beta;
    retp(yd);
endp;

proc GLS_bt(y,tau,cbar);
    local n,tb,y_ers,dt,ctdt,z,beta,yd;
	n = rows(y);
	tb = floor(tau*n);
    y_ers = y[1]|(y[2:n]-(1+(cbar/n))*y[1:n-1]);
	dt = zeros(tb,1)|seqa(1,1,n-tb);
	ctdt = ones(n,1)~seqa(1,1,n)~dt;
	z = ctdt[1,.]|(ctdt[2:n,.]-(1+(cbar/n))*ctdt[1:n-1,.]);
	beta = inv(z'z)*z'y_ers;
	yd = y-ctdt*beta;
    retp(yd);
endp;

proc DF(r,k);
	local dr,x,dep,invxx,b,res,s2,adf;
	dr = r-lag1(r);
	if k == 0;
		x = trimr(lag1(r),1,0);
	else;
		x = trimr(lag1(r)~shiftr(dr',seqa(1,1,k),0)',k+1,0);
	endif;
	dep = trimr(dr,k+1,0);
	invxx = inv(x'x);
	b = invxx*x'dep;
	res = dep-x*b;
	s2 = res'res/(rows(x)-cols(x));
	adf = b[1]/sqrt(s2*invxx[1,1]);
	retp(adf);
endp;

proc (3) = MZ(r,k);
	local n,dr,x,dep,invxx,b,res,sig2,s2,sumr2,mza,msb,mzt;
	n = rows(r);
	dr = r-lag1(r);
	if k == 0;
		x = trimr(lag1(r),1,0);
	else;
		x = trimr(lag1(r)~shiftr(dr',seqa(1,1,k),0)',k+1,0);
	endif;
	dep = trimr(dr,k+1,0);
	invxx = inv(x'x);
	b = invxx*x'dep;
	res = dep-x*b;
	sig2 = res'res/(rows(x)-cols(x));
	if k == 0;
		s2 = sig2;
	else;
		s2 = sig2/((1-(sumc(b[2:k+1])))^2);
	endif;
	sumr2 = r[1:n-1]'r[1:n-1];
	mza = (((r[n]^2)/n)-s2)/(2*sumr2/(n^2));
	msb = sqrt(sumr2/((n^2)*s2));
	mzt = mza*msb;
	retp(mza,msb,mzt);
endp;

proc bart(u,l);
	local n,p,j,s2l;
    n = rows(u);
	p = zeros(l,1);
	j = 1;
	do while j <= l;
		p[j] = (1-(j/(l+1)))*sumc(u[j+1:n].*u[1:n-j]);
	j = j + 1;
	endo;
	s2l = (sumc(u.^2)+(2*sumc(p)))/n;
	retp(s2l);
endp;

proc KPSS(r,s2);
    local stat;
    stat = sumc(cumsumc(r).^2)/((rows(r)^2)*s2);
    retp(stat);
endp;

proc kMAIC(r,kmax);
	local n,maic,k,dr,x,dep,invxx,b,res,s2,tauk,kopt;
	n = rows(r);
	maic = zeros(kmax+1,1);
	k = 0;
	do while k <= kmax;
		dr = r-lag1(r);
		if k == 0;
			x = trimr(lag1(r),kmax+1,0);
		else;
			x = trimr(lag1(r)~shiftr(dr',seqa(1,1,k),0)',kmax+1,0);
		endif;
		dep = trimr(dr,kmax+1,0);
		invxx = inv(x'x);
		b = invxx*x'dep;
		res = dep-x*b;
		s2 = res'res/(n-kmax-1);
		tauk = (b[1]^2)*(x[.,1]'x[.,1])/s2;
		maic[k+1] = ln(s2)+(2*(tauk+k)/(n-kmax-1));
	k = k + 1;
	endo;
	kopt = minindc(maic)-1;
retp(kopt);
endp;

end;
