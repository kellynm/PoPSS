#include <Rcpp.h>
// [[Rcpp::depends(RcppParallel)]]
#include <RcppParallel.h>
#include <omp.h>
using namespace Rcpp;
using namespace RcppParallel;
// [[Rcpp::plugins(openmp)]]

//Within each infected cell (I > 0) draw random number of infections ~Poisson(lambda=rate of spore production) for each infected host. 
//Take SUM for total infections produced by each cell. 

// [[Rcpp::export]]
IntegerMatrix SporeGenCpp(IntegerMatrix infected, NumericMatrix weather_suitability, double rate){
  
  // internal variables
  int nrow = infected.nrow(); 
  int ncol = infected.ncol();
  
  IntegerMatrix SP = clone<IntegerMatrix>(infected);
  //Function rpois("rpois");
  
  RNGScope scope;
  
  // LOOP THROUGH EACH INFECTED CELL AND GENERATE AMOUNT OF SPORES
  //#pragma omp parallel for
  for (int row = 0; row < nrow; row++) {
    for (int col = 0; col < ncol; col++){
      
      if (infected(row, col) > 0){  //if infected > 0, generate spores proportional to production rate * weather suitability
        double lambda = rate * weather_suitability(row, col);
        NumericVector inf = rpois(infected(row, col), lambda);
        int s = sum(inf);
        SP(row, col) = s;
      }
    }
  }
  return SP;
}

// [[Rcpp::export]]
List SporeDispCpp_mh(IntegerMatrix spore_matrix,
                     IntegerMatrix S_host1_mat, IntegerMatrix S_host2_mat,
                     IntegerMatrix I_host1_mat, IntegerMatrix I_host2_mat, 
                     IntegerMatrix N_LVE, NumericMatrix weather_suitability,   //use different name than the functions in myfunctions_SOD.r
                double rs, String rtype, double scale1,
                IntegerMatrix S_host3_mat, IntegerMatrix I_host3_mat,
                IntegerMatrix S_host4_mat, IntegerMatrix I_host4_mat,
                IntegerMatrix S_host5_mat, IntegerMatrix I_host5_mat,
                IntegerMatrix S_host6_mat, IntegerMatrix I_host6_mat,
                IntegerMatrix S_host7_mat, IntegerMatrix I_host7_mat,
                IntegerMatrix S_host8_mat, IntegerMatrix I_host8_mat,
                IntegerMatrix S_host9_mat, IntegerMatrix I_host9_mat,
                IntegerMatrix S_host10_mat, IntegerMatrix I_host10_mat,
                double scale2=NA_REAL,  //default values
                double gamma=NA_REAL){  //default values

  // internal variables //
  int nrow = spore_matrix.nrow(); 
  int ncol = spore_matrix.ncol();
  int row0;
  int col0;

  double dist;
  double theta;
  double PropS;

  //for Rcpp random numbers
  RNGScope scope;
  
  Function sample("sample");
  //Function rcauchy("rcauchy");  

  //LOOP THROUGH EACH CELL of the input matrix 'spore_matrix' (this should be the study area)
  for (int row = 0; row < nrow; row++) {
    for (int col = 0; col < ncol; col++){
      
      if(spore_matrix(row,col) > 0){  //if spores in cell (row,col) > 0, disperse
        
        for(int sp = 1; (sp <= spore_matrix(row,col)); sp++){
          
          //GENERATE DISTANCES:
          if (rtype == "Cauchy") 
            dist = abs(R::rcauchy(0, scale1));
          else if (rtype == "Cauchy Mixture"){
            if (gamma >= 1 || gamma <= 0) stop("The parameter gamma must range between (0-1)");
            NumericVector fv = sample(Range(1, 2), 1, false, NumericVector::create(gamma, 1-gamma));
            int f = fv[0];
            if(f == 1) 
              dist = abs(R::rcauchy(0, scale1));
            else 
              dist = abs(R::rcauchy(0, scale2));
          }
          else 
            stop("The parameter rtype must be set to either 'Cauchy' or 'Cauchy Mixture'");
        
          //GENERATE ANGLES (using Uniform distribution):
          theta = R::runif(-PI, PI);
          
          //calculate new row and col position for the dispersed spore unit (using dist and theta)
          row0 = row - round((dist * cos(theta)) / rs);
          col0 = col + round((dist * sin(theta)) / rs);
          
          
          if (row0 < 0 || row0 >= nrow) continue;     //outside the region
          if (col0 < 0 || col0 >= ncol) continue;     //outside the region
          
          //if distance is within same pixel challenge all SOD hosts, otherwise challenge UMCA only
          if (row0 == row && col0 == col){
            
            //if susceptible hosts are present in cell, calculate prob of infection
            if(S_host1_mat(row0, col0) > 0 || S_host2_mat(row0, col0) > 0){
  		        
              PropS = double(S_host1_mat(row0, col0) + S_host2_mat(row0, col0)) / N_LVE(row0, col0);            
              
              double U = R::runif(0,1);
              double Prob = PropS * weather_suitability(row0, col0); //weather suitability affects prob success!

              //if U < Prob then one host will become infected
              if (U < Prob){
                
                double prop_S_host1 = double(S_host1_mat(row0, col0)) / (S_host1_mat(row0, col0) + S_host2_mat(row0, col0)); //fractions of susceptible host in cell
                double prop_S_host2 = double(S_host2_mat(row0, col0)) / (S_host1_mat(row0, col0) + S_host2_mat(row0, col0));
                
                //sample which of the three hosts will be infected
                NumericVector sv = sample(NumericVector::create(1, 2), 1, false, NumericVector::create(prop_S_host1, prop_S_host2));
                int s = sv[0];
                if (s == 1){
                  I_host1_mat(row0, col0) = I_host1_mat(row0, col0) + 1; //update infected UMCA
                  S_host1_mat(row0, col0) = S_host1_mat(row0, col0) - 1; //update susceptible UMCA                                    
                }else{
                  I_host2_mat(row0, col0) = I_host2_mat(row0, col0) + 1; //update infected QUKE
                  S_host2_mat(row0, col0) = S_host2_mat(row0, col0) - 1; //update susceptible QUKE                    
                } 
              }//ENF IF INFECTION LEVEL II 
              
            }//ENF IF          
          
          }else{

            //if UMCA-only susceptibles are present in cell, calculate prob of infection
            if(S_host1_mat(row0, col0) > 0){
              double prop_S_host1 = double(S_host1_mat(row0, col0)) / N_LVE(row0, col0); //fractions of given host in cell
              double U = R::runif(0,1);
              double Prob = prop_S_host1 * weather_suitability(row0, col0); //weather suitability affects prob success!
              //if U < Prob then one host will become infected
              if (U < Prob){
                I_host1_mat(row0, col0) = I_host1_mat(row0, col0) + 1; //update infected UMCA
                S_host1_mat(row0, col0) = S_host1_mat(row0, col0) - 1; //update susceptible UMCA             
              }  
            }//END IF
          }//ENF IF DISTANCE CHECK  
          
        
        }//END LOOP OVER ALL SPORES IN CURRENT CELL GRID
       
       
      }//END IF  
      
    }   
  }//END LOOP OVER ALL GRID CELLS

  //return List::create(Named("I")=I, Named("S")=S);
  return List::create(
    _["S_host1_mat"] = S_host1_mat, 
    _["I_host1_mat"] = I_host1_mat,
    _["S_host2_mat"] = S_host2_mat, 
    _["I_host2_mat"] = I_host2_mat   
  );
  
}

// [[Rcpp::export]]
List SporeDispCppWind_mh(IntegerMatrix spore_matrix, 
                         IntegerMatrix S_host1_mat, IntegerMatrix S_host2_mat,
                         IntegerMatrix I_host1_mat, IntegerMatrix I_host2_mat, 
                         IntegerMatrix N_LVE, NumericMatrix weather_suitability,   //use different name than the functions in myfunctions_SOD.r
                double rs, String rtype, double scale1, 
                String wdir, int kappa,
                IntegerMatrix S_host3_mat, IntegerMatrix I_host3_mat,
                IntegerMatrix S_host4_mat, IntegerMatrix I_host4_mat,
                IntegerMatrix S_host5_mat, IntegerMatrix I_host5_mat,
                IntegerMatrix S_host6_mat, IntegerMatrix I_host6_mat,
                IntegerMatrix S_host7_mat, IntegerMatrix I_host7_mat,
                IntegerMatrix S_host8_mat, IntegerMatrix I_host8_mat,
                IntegerMatrix S_host9_mat, IntegerMatrix I_host9_mat,
                IntegerMatrix S_host10_mat, IntegerMatrix I_host10_mat,
                double scale2=NA_REAL,  //default values
                double gamma=NA_REAL){  //default values

  // internal variables //
  int nrow = spore_matrix.nrow(); 
  int ncol = spore_matrix.ncol();
  int row0;
  int col0;

  double dist;
  double theta;
  double PropS;
  double total_hosts;
  
  //for Rcpp random numbers
  RNGScope scope;
  
  //Function rcauchy("rcauchy");
  Function rvm("rvm");
  Function sample("sample");

  //LOOP THROUGH EACH CELL of the input matrix 'spore_matrix' (this should be the study area)
  for (int row = 0; row < nrow; row++) {
    for (int col = 0; col < ncol; col++){
      
      if(spore_matrix(row,col) > 0){  //if spores in cell (row,col) > 0, disperse
        
        for(int sp = 1; (sp <= spore_matrix(row,col)); sp++){
          
          //GENERATE DISTANCES:
          if (rtype == "Cauchy") 
            dist = abs(R::rcauchy(0, scale1));
          else if (rtype == "Cauchy Mixture"){
            if (gamma >= 1 || gamma <= 0) stop("The parameter gamma must range between (0-1)");
            NumericVector fv = sample(Range(1, 2), 1, false, NumericVector::create(gamma, 1-gamma));
            int f = fv[0];
            if(f == 1) 
              dist = abs(R::rcauchy(0, scale1));
            else 
              dist = abs(R::rcauchy(0, scale2));
          }
          else 
            stop("The parameter rtype must be set to either 'Cauchy' or 'Cauchy Mixture'");
        
          //GENERATE ANGLES (using Von Mises distribution):
          if(kappa <= 0)  // kappa=concentration
            stop("kappa must be greater than zero!");
          
          //predominant wind dir
          if (wdir == "N") 
            theta = as<double>(rvm(1, 0 * (PI/180), kappa));  
          else if (wdir == "NE")
            theta = as<double>(rvm(1, 45 * (PI/180), kappa));  
          else if(wdir == "E")
            theta = as<double>(rvm(1, 90 * (PI/180), kappa));  
          else if(wdir == "SE")
            theta = as<double>(rvm(1, 135 * (PI/180), kappa));  
          else if(wdir == "S")
            theta = as<double>(rvm(1, 180 * (PI/180), kappa));  
          else if(wdir == "SW")
            theta = as<double>(rvm(1, 225 * (PI/180), kappa));  
          else if(wdir == "W")
            theta = as<double>(rvm(1, 270 * (PI/180), kappa));  
          else
            theta = as<double>(rvm(1, 315 * (PI/180), kappa));

          
          //calculate new row and col position for the dispersed spore unit (using dist and theta)
          row0 = row - round((dist * cos(theta)) / rs);
          col0 = col + round((dist * sin(theta)) / rs);
          
          if (row0 < 0 || row0 >= nrow) continue;     //outside of the study area
          if (col0 < 0 || col0 >= ncol) continue;     //outside of the study area
          
          //if distance is within same pixel challenge all hosts, otherwise challenge only hosts with score above 0
          if (row0 == row && col0 == col){
            
            //if susceptible hosts are present in cell, calculate prob of infection
            if(S_host1_mat(row0, col0) > 0 || S_host2_mat(row0, col0) > 0 || S_host3_mat(row0, col0) > 0 || S_host4_mat(row0, col0) > 0 || S_host5_mat(row0, col0) > 0 ||
               S_host6_mat(row0, col0) > 0 || S_host7_mat(row0, col0) > 0 || S_host8_mat(row0, col0) > 0 || S_host9_mat(row0, col0) > 0 || S_host10_mat(row0, col0) > 0 ){
    	        
    	        total_hosts = double(S_host1_mat(row0, col0) + S_host2_mat(row0, col0) + S_host3_mat(row0, col0) + S_host4_mat(row0, col0) + S_host5_mat(row0, col0) +
    	          S_host6_mat(row0, col0) + S_host7_mat(row0, col0) + S_host8_mat(row0, col0) + S_host9_mat(row0, col0) + S_host10_mat(row0, col0));
              PropS = total_hosts / N_LVE(row0, col0);;              
              
              double U = R::runif(0,1);
              double Prob = PropS * weather_suitability(row0, col0); //weather suitability affects prob success!

              //if U < Prob then one host will become infected
              if (U < Prob){
                
                double prop_S_host1 = double(S_host1_mat(row0, col0)) / total_hosts; //fractions of susceptible host in cell
                double prop_S_host2 = double(S_host2_mat(row0, col0)) / total_hosts;
                double prop_S_host3 = double(S_host3_mat(row0, col0)) / total_hosts;
                double prop_S_host4 = double(S_host4_mat(row0, col0)) / total_hosts;
                double prop_S_host5 = double(S_host5_mat(row0, col0)) / total_hosts;
                double prop_S_host6 = double(S_host6_mat(row0, col0)) / total_hosts;
                double prop_S_host7 = double(S_host7_mat(row0, col0)) / total_hosts;
                double prop_S_host8 = double(S_host8_mat(row0, col0)) / total_hosts;
                double prop_S_host9 = double(S_host9_mat(row0, col0)) / total_hosts;
                double prop_S_host10 = double(S_host10_mat(row0, col0)) / total_hosts;
                
                //sample which host will be infected
                NumericVector sv = sample(seq_len(10), 1, false, NumericVector::create(prop_S_host1, prop_S_host2,prop_S_host3, prop_S_host4, prop_S_host5,prop_S_host6,prop_S_host7,prop_S_host8,prop_S_host9,prop_S_host10));
                int s = sv[0];
                if (s == 1){
                  I_host1_mat(row0, col0) = I_host1_mat(row0, col0) + 1; //update infected host 1
                  S_host1_mat(row0, col0) = S_host1_mat(row0, col0) - 1; //update susceptible host 1                                    
                }else if (s == 2){
                  I_host2_mat(row0, col0) = I_host2_mat(row0, col0) + 1; //update infected host 2
                  S_host2_mat(row0, col0) = S_host2_mat(row0, col0) - 1; //update susceptible host 2
                }else if (s == 3){
                  I_host2_mat(row0, col0) = I_host2_mat(row0, col0) + 1; //update infected host 3
                  S_host2_mat(row0, col0) = S_host2_mat(row0, col0) - 1; //update susceptible host 3
                }else if (s == 4){
                  I_host2_mat(row0, col0) = I_host2_mat(row0, col0) + 1; //update infected host 4
                  S_host2_mat(row0, col0) = S_host2_mat(row0, col0) - 1; //update susceptible host 4
                }else if (s == 5){
                  I_host2_mat(row0, col0) = I_host2_mat(row0, col0) + 1; //update infected host 5
                  S_host2_mat(row0, col0) = S_host2_mat(row0, col0) - 1; //update susceptible host 5
                }else if (s == 6){
                  I_host2_mat(row0, col0) = I_host2_mat(row0, col0) + 1; //update infected host 6
                  S_host2_mat(row0, col0) = S_host2_mat(row0, col0) - 1; //update susceptible host 6
                }else if (s == 7){
                  I_host2_mat(row0, col0) = I_host2_mat(row0, col0) + 1; //update infected host 7
                  S_host2_mat(row0, col0) = S_host2_mat(row0, col0) - 1; //update susceptible host 7
                }else if (s == 8){
                  I_host2_mat(row0, col0) = I_host2_mat(row0, col0) + 1; //update infected host 8
                  S_host2_mat(row0, col0) = S_host2_mat(row0, col0) - 1; //update susceptible host 8
                }else if (s == 9){
                  I_host2_mat(row0, col0) = I_host2_mat(row0, col0) + 1; //update infected host 9
                  S_host2_mat(row0, col0) = S_host2_mat(row0, col0) - 1; //update susceptible host 9
                }else if (s == 10){
                  I_host2_mat(row0, col0) = I_host2_mat(row0, col0) + 1; //update infected host 10
                  S_host2_mat(row0, col0) = S_host2_mat(row0, col0) - 1; //update susceptible host 0
                }      
              }//ENF IF INFECTION LEVEL II 
              
            }//ENF IF          
          
          }else{

            //if Placeholder for challengign only host above 0
            if(S_host1_mat(row0, col0) > 0 || S_host2_mat(row0, col0) > 0 || S_host3_mat(row0, col0) > 0 || S_host4_mat(row0, col0) > 0 || S_host5_mat(row0, col0) > 0 ||
               S_host6_mat(row0, col0) > 0 || S_host7_mat(row0, col0) > 0 || S_host8_mat(row0, col0) > 0 || S_host9_mat(row0, col0) > 0 || S_host10_mat(row0, col0) > 0 ){
              
              total_hosts = double(S_host1_mat(row0, col0) + S_host2_mat(row0, col0) + S_host3_mat(row0, col0) + S_host4_mat(row0, col0) + S_host5_mat(row0, col0) +
                S_host6_mat(row0, col0) + S_host7_mat(row0, col0) + S_host8_mat(row0, col0) + S_host9_mat(row0, col0) + S_host10_mat(row0, col0));
              PropS = total_hosts / N_LVE(row0, col0);;              
              
              double U = R::runif(0,1);
              double Prob = PropS * weather_suitability(row0, col0); //weather suitability affects prob success!
              
              //if U < Prob then one host will become infected
              if (U < Prob){
                double prop_S_host1 = double(S_host1_mat(row0, col0)) / total_hosts; //fractions of susceptible host in cell
                double prop_S_host2 = double(S_host2_mat(row0, col0)) / total_hosts;
                double prop_S_host3 = double(S_host3_mat(row0, col0)) / total_hosts;
                double prop_S_host4 = double(S_host4_mat(row0, col0)) / total_hosts;
                double prop_S_host5 = double(S_host5_mat(row0, col0)) / total_hosts;
                double prop_S_host6 = double(S_host6_mat(row0, col0)) / total_hosts;
                double prop_S_host7 = double(S_host7_mat(row0, col0)) / total_hosts;
                double prop_S_host8 = double(S_host8_mat(row0, col0)) / total_hosts;
                double prop_S_host9 = double(S_host9_mat(row0, col0)) / total_hosts;
                double prop_S_host10 = double(S_host10_mat(row0, col0)) / total_hosts;
                
                //sample which host will be infected
                NumericVector sv = sample(seq_len(10), 1, false, NumericVector::create(prop_S_host1, prop_S_host2,prop_S_host3, prop_S_host4, prop_S_host5,prop_S_host6,prop_S_host7,prop_S_host8,prop_S_host9,prop_S_host10));
                int s = sv[0];
                if (s == 1){
                  I_host1_mat(row0, col0) = I_host1_mat(row0, col0) + 1; //update infected host 1
                  S_host1_mat(row0, col0) = S_host1_mat(row0, col0) - 1; //update susceptible host 1                                    
                }else if (s == 2){
                  I_host2_mat(row0, col0) = I_host2_mat(row0, col0) + 1; //update infected host 2
                  S_host2_mat(row0, col0) = S_host2_mat(row0, col0) - 1; //update susceptible host 2
                }else if (s == 3){
                  I_host2_mat(row0, col0) = I_host2_mat(row0, col0) + 1; //update infected host 3
                  S_host2_mat(row0, col0) = S_host2_mat(row0, col0) - 1; //update susceptible host 3
                }else if (s == 4){
                  I_host2_mat(row0, col0) = I_host2_mat(row0, col0) + 1; //update infected host 4
                  S_host2_mat(row0, col0) = S_host2_mat(row0, col0) - 1; //update susceptible host 4
                }else if (s == 5){
                  I_host2_mat(row0, col0) = I_host2_mat(row0, col0) + 1; //update infected host 5
                  S_host2_mat(row0, col0) = S_host2_mat(row0, col0) - 1; //update susceptible host 5
                }else if (s == 6){
                  I_host2_mat(row0, col0) = I_host2_mat(row0, col0) + 1; //update infected host 6
                  S_host2_mat(row0, col0) = S_host2_mat(row0, col0) - 1; //update susceptible host 6
                }else if (s == 7){
                  I_host2_mat(row0, col0) = I_host2_mat(row0, col0) + 1; //update infected host 7
                  S_host2_mat(row0, col0) = S_host2_mat(row0, col0) - 1; //update susceptible host 7
                }else if (s == 8){
                  I_host2_mat(row0, col0) = I_host2_mat(row0, col0) + 1; //update infected host 8
                  S_host2_mat(row0, col0) = S_host2_mat(row0, col0) - 1; //update susceptible host 8
                }else if (s == 9){
                  I_host2_mat(row0, col0) = I_host2_mat(row0, col0) + 1; //update infected host 9
                  S_host2_mat(row0, col0) = S_host2_mat(row0, col0) - 1; //update susceptible host 9
                }else if (s == 10){
                  I_host2_mat(row0, col0) = I_host2_mat(row0, col0) + 1; //update infected host 10
                  S_host2_mat(row0, col0) = S_host2_mat(row0, col0) - 1; //update susceptible host 0
                }  
            }//END IF
            }
          }//END IF DISTANCE CHECK  
        
        }//END LOOP OVER ALL SPORES IN CURRENT CELL GRID
       
       
      }//END IF 
      
    }   
  }//END LOOP OVER ALL GRID CELLS

  //return List::create(Named("I")=I, Named("S")=S);
  return List::create(
    _["S_host1_mat"] = S_host1_mat, 
    _["I_host1_mat"] = I_host1_mat,
    _["S_host2_mat"] = S_host2_mat, 
    _["I_host2_mat"] = I_host2_mat   
  );
}

// [[Rcpp::export]]
List SporeDispCpp_MH(IntegerMatrix spore_matrix, 
                     IntegerMatrix S_host1_mat, IntegerMatrix S_LD, IntegerMatrix S_host2_mat, 
                     IntegerMatrix I_host1_mat, IntegerMatrix I_LD, IntegerMatrix I_host2_mat, 
                     IntegerMatrix N_LVE,
                     NumericMatrix W,   //use different name than the functions in myfunctions_SOD.r
                     double rs, String rtype, double scale1, 
                     double scale2=NA_REAL,  //default values
                     double gamma=NA_REAL)	//default values
{  
  
  // internal variables //
  int nrow = spore_matrix.nrow(); 
  int ncol = spore_matrix.ncol();
  int row0;
  int col0;
  
  double dist;
  double theta;
  double PropS;
  
  //for Rcpp random numbers
  RNGScope scope;
  
  Function sample("sample");
  //Function rcauchy("rcauchy");  
  
  //LOOP THROUGH EACH CELL of the input matrix 'spore_matrix' (this should be the study area)
  for (int row = 0; row < nrow; row++) {
    for (int col = 0; col < ncol; col++){
      
      if(spore_matrix(row,col) > 0){  //if spores in cell (row,col) > 0, disperse
        
        for(int sp = 1; (sp <= spore_matrix(row,col)); sp++){
          
          //GENERATE DISTANCES:
          if (rtype == "Cauchy") 
            dist = abs(R::rcauchy(0, scale1));
          else if (rtype == "Cauchy Mixture"){
            if (gamma >= 1 || gamma <= 0) stop("The parameter gamma must range between (0-1)");
            NumericVector fv = sample(Range(1, 2), 1, false, NumericVector::create(gamma, 1-gamma));
            int f = fv[0];
            if(f == 1) 
              dist = abs(R::rcauchy(0, scale1));
            else 
              dist = abs(R::rcauchy(0, scale2));
          }
          else stop("The parameter rtype must be set to either 'Cauchy' or 'Cauchy Mixture'");
          
          //GENERATE ANGLES (using Uniform distribution):
          theta = R::runif(-PI, PI);
          
          //calculate new row and col position for the dispersed spore unit (using dist and theta)
          row0 = row - round((dist * cos(theta)) / rs);
          col0 = col + round((dist * sin(theta)) / rs);
          
          if (row0 < 0 || row0 >= nrow) continue;     //outside the region
          if (col0 < 0 || col0 >= ncol) continue;     //outside the region
          
          //if distance is within same pixel challenge all SOD hosts, otherwise challenge UMCA only
          if (row0 == row && col0 == col){
            
            //if susceptible hosts are present in cell, calculate prob of infection
            if(S_host1_mat(row0, col0) > 0 || S_host2_mat(row0, col0) > 0 || S_LD(row0, col0) > 0 ){
              
              //WHAT IS THE PROBABILITY THAT A SPORE HITS ANY SUSCEPTIBLE HOST?
              int S_TOTAL = S_host1_mat(row0, col0) + S_host2_mat(row0, col0) + S_LD(row0, col0);
              
              PropS = double (S_TOTAL) / N_LVE(row0, col0);
              
              double U = R::runif(0,1);			  
              
              //if U < PropS then one of the susceptible trees will get hit
              if (U < PropS){
                
                //WHICH SUSCEPTIBLE TREE GETS HIT? CALCULATE PROBABILITY WEIGHTS
                double Prob_UM = double (S_host1_mat(row0, col0)) / S_TOTAL; 
                double Prob_LD = double (S_LD(row0, col0)) / S_TOTAL;
                double Prob_OK = double (S_host2_mat(row0, col0)) / S_TOTAL;
                
                //sample which of the three hosts will be hit given probability weights
                IntegerVector sv = sample(seq_len(3), 1, false, 
                                          NumericVector::create(Prob_UM, Prob_LD, Prob_OK));
                
                //WHAT IS THE PROBABILITY THAT A SPORE TURNS INTO AN INFECTION, GIVEN THAT A SUSCEPTIBLE TREE HAS BEEN HIT?
                //This depends on both the local weather AND the host competency score (relative to UMCA and OAKS)
                int s = sv[0];
                if (s == 1){
                  double ProbINF = Prob_UM * W(row0, col0);
                  if (U < ProbINF){
                    I_host1_mat(row0, col0) = I_host1_mat(row0, col0) + 1; //update infected UMCA
                    S_host1_mat(row0, col0) = S_host1_mat(row0, col0) - 1; //update susceptible UMCA
                  }
                }else if (s == 2){
                  double ProbINF = Prob_LD * W(row0, col0);
                  if (U < ProbINF){
                    I_LD(row0, col0) = I_LD(row0, col0) + 1; //update infected LIDE
                    S_LD(row0, col0) = S_LD(row0, col0) - 1; //update susceptible LIDE                    
                  }
                }else{
                  double ProbINF = Prob_OK * W(row0, col0) * 0.75; //hardcoded coeff to decrease prob.infection (transmission)
                  if (U < ProbINF){
                    I_host2_mat(row0, col0) = I_host2_mat(row0, col0) + 1; //update infected OAKS
                    S_host2_mat(row0, col0) = S_host2_mat(row0, col0) - 1; //update susceptible OAKS  				
                  }
                }
              }//END IF CHECK vs UNIFORM NUMBER	                
              
            }//END IF NO SUSCEPTIBLE PRESENT IN CELL
            
          }else{  //IF DISTANCE IS OUTSIDE THE SAME CELL
            
            if(S_host1_mat(row0, col0) > 0 || S_LD(row0, col0) > 0){  //IF SUSCEPTIBLE HOST IS AVAILABLE (UMCA OR LIDE)
              
              //WHAT IS THE PROBABILITY THAT A SPORE HITS ANY SUSCEPTIBLE HOST?
              PropS = double(S_host1_mat(row0, col0) + S_LD(row0, col0)) / N_LVE(row0, col0);
              
              double U = R::runif(0,1);		
              
              //if U < ProbS then one of the susceptible trees will get hit
              if (U < PropS){
                
                //WHICH SUSCEPTIBLE TREE GETS HIT? CALCULATE PROBABILITY WEIGHTS
                double Prob_UM = double (S_host1_mat(row0, col0)) / (S_host1_mat(row0, col0) + S_LD(row0, col0)); 
                double Prob_LD = double (S_LD(row0, col0)) / (S_host1_mat(row0, col0) + S_LD(row0, col0));
                
                //sample which of the three hosts will be hit given probability weights
                IntegerVector sv = sample(seq_len(2), 1, false, 
                                          NumericVector::create(Prob_UM, Prob_LD));
                
                //WHAT IS THE PROBABILITY THAT A SPORE TURNS INTO AN INFECTION, GIVEN THAT A SUSCEPTIBLE TREE HAS BEEN HIT?
                //This depends on both the local weather AND the host competency score (relative to UMCA and OAKS)
                int s = sv[0];
                if (s == 1){
                  double ProbINF = Prob_UM * W(row0, col0);
                  if (U < ProbINF){
                    I_host1_mat(row0, col0) = I_host1_mat(row0, col0) + 1; //update infected UMCA
                    S_host1_mat(row0, col0) = S_host1_mat(row0, col0) - 1; //update susceptible UMCA
                  }
                }else{
                  double ProbINF = Prob_LD * W(row0, col0);
                  if (U < ProbINF){
                    I_LD(row0, col0) = I_LD(row0, col0) + 1; //update infected LIDE
                    S_LD(row0, col0) = S_LD(row0, col0) - 1; //update susceptible LIDE                    
                  }                
                }  
              }//END IF U < PROB
            }//ENF IF S > 0  
          } //END IF DISTANCE CHECK
          
        }//END LOOP OVER ALL SPORES IN CURRENT CELL GRID     
      }//END IF SPORES IN CURRENT CELL
    }   
  }//END LOOP OVER ALL GRID CELLS
  
  //return List::create(Named("I")=I, Named("S")=S);
  return List::create(
    _["S_host1_mat"] = S_host1_mat, 
    _["I_host1_mat"] = I_host1_mat,
    _["S_host2_mat"] = S_host2_mat, 
    _["I_host2_mat"] = I_host2_mat,
    _["S_LD"] = S_LD, 
    _["I_LD"] = I_LD
  );
  
} //END OF FUNCTION				  

// [[Rcpp::export]]
List SporeDispCppWind_MH(IntegerMatrix spore_matrix, 
                         IntegerMatrix S_host1_mat, IntegerMatrix S_LD, IntegerMatrix S_host2_mat, 
                         IntegerMatrix I_host1_mat, IntegerMatrix I_LD, IntegerMatrix I_host2_mat, 
                         IntegerMatrix N_LVE,
                         NumericMatrix W,   //use different name than the functions in myfunctions_SOD.r
                         double rs, String rtype, double scale1, 
                         String wdir, int kappa,
                         double scale2=NA_REAL,  //default values
                         double gamma=NA_REAL)   //default values
{ 
  
  // internal variables //
  int nrow = spore_matrix.nrow(); 
  int ncol = spore_matrix.ncol();
  int row0;
  int col0;
  
  double dist;
  double theta;
  double PropS;
  
  //for Rcpp random numbers
  RNGScope scope;
  
  //Function rcauchy("rcauchy");
  Function rvm("rvm");
  Function sample("sample");
  
  //LOOP THROUGH EACH CELL of the input matrix 'spore_matrix' (this should be the study area)
  for (int row = 0; row < nrow; row++) {
    for (int col = 0; col < ncol; col++){
      
      if(spore_matrix(row,col) > 0){  //if spores in cell (row,col) > 0, disperse
        
        for(int sp = 1; (sp <= spore_matrix(row,col)); sp++){
          
          //GENERATE DISTANCES:
          if (rtype == "Cauchy") 
            dist = abs(R::rcauchy(0, scale1));
          else if (rtype == "Cauchy Mixture"){
            if (gamma >= 1 || gamma <= 0) stop("The parameter gamma must range between (0-1)");
            NumericVector fv = sample(Range(1, 2), 1, false, NumericVector::create(gamma, 1-gamma));
            int f = fv[0];
            if(f == 1) 
              dist = abs(R::rcauchy(0, scale1));
            else 
              dist = abs(R::rcauchy(0, scale2));
          }
          else stop("The parameter rtype must be set to either 'Cauchy' or 'Cauchy Mixture'");
          
          //GENERATE ANGLES (using Von Mises distribution):
          if(kappa <= 0)  // kappa=concentration
            stop("kappa must be greater than zero!");
          
          //predominant wind dir
          if (wdir == "N") 
            theta = as<double>(rvm(1, 0 * (PI/180), kappa));  
          else if (wdir == "NE")
            theta = as<double>(rvm(1, 45 * (PI/180), kappa));  
          else if(wdir == "E")
            theta = as<double>(rvm(1, 90 * (PI/180), kappa));  
          else if(wdir == "SE")
            theta = as<double>(rvm(1, 135 * (PI/180), kappa));  
          else if(wdir == "S")
            theta = as<double>(rvm(1, 180 * (PI/180), kappa));  
          else if(wdir == "SW")
            theta = as<double>(rvm(1, 225 * (PI/180), kappa));  
          else if(wdir == "W")
            theta = as<double>(rvm(1, 270 * (PI/180), kappa));  
          else
            theta = as<double>(rvm(1, 315 * (PI/180), kappa));
          
          //calculate new row and col position for the dispersed spore unit (using dist and theta)
          row0 = row - round((dist * cos(theta)) / rs);
          col0 = col + round((dist * sin(theta)) / rs);
          
          if (row0 < 0 || row0 >= nrow) continue;     //outside the region
          if (col0 < 0 || col0 >= ncol) continue;     //outside the region
          
          //if distance is within same pixel challenge all SOD hosts, otherwise challenge UMCA only
          if (row0 == row && col0 == col){
            
            //if susceptible hosts are present in cell, calculate prob of infection
            if(S_host1_mat(row0, col0) > 0 || S_host2_mat(row0, col0) > 0 || S_LD(row0, col0) > 0){
              
              //WHAT IS THE PROBABILITY THAT A SPORE HITS ANY SUSCEPTIBLE HOST?
              int S_TOTAL = S_host1_mat(row0, col0) + S_host2_mat(row0, col0) + S_LD(row0, col0);
              
              PropS = double (S_TOTAL) / N_LVE(row0, col0);
              
              double U = R::runif(0,1);			  
              
              //if U < PropS then one of the susceptible trees will get hit
              if (U < PropS){
                
                //WHICH SUSCEPTIBLE TREE GETS HIT? CALCULATE PROBABILITY WEIGHTS
                double Prob_UM = double (S_host1_mat(row0, col0)) / S_TOTAL; 
                double Prob_LD = double (S_LD(row0, col0)) / S_TOTAL;
                double Prob_OK = double (S_host2_mat(row0, col0)) / S_TOTAL;
                
                //sample which of the three hosts will be hit given probability weights
                IntegerVector sv = sample(seq_len(3), 1, false, 
                                          NumericVector::create(Prob_UM, Prob_LD, Prob_OK));
                
                int s = sv[0];
                if (s == 1){
                  double ProbINF = Prob_UM * W(row0, col0);
                  if (U < ProbINF){
                    I_host1_mat(row0, col0) = I_host1_mat(row0, col0) + 1; //update infected UMCA
                    S_host1_mat(row0, col0) = S_host1_mat(row0, col0) - 1; //update susceptible UMCA
                  }
                }else if (s == 2){
                  double ProbINF = Prob_LD * W(row0, col0);
                  if (U < ProbINF){
                    I_LD(row0, col0) = I_LD(row0, col0) + 1; //update infected LIDE
                    S_LD(row0, col0) = S_LD(row0, col0) - 1; //update susceptible LIDE                    
                  }
                }else{
                  double ProbINF = Prob_OK * W(row0, col0) * 0.75; //hardcoded coeff to decrease prob.infection (transmission)
                  if (U < ProbINF){
                    I_host2_mat(row0, col0) = I_host2_mat(row0, col0) + 1; //update infected OAKS
                    S_host2_mat(row0, col0) = S_host2_mat(row0, col0) - 1; //update susceptible OAKS  				
                  }
                }
              }//END IF CHECK vs UNIFORM NUMBER	                
              
            }//END IF NO SUSCEPTIBLE PRESENT IN CELL
            
          }else{  //IF DISTANCE IS OUTSIDE THE SAME CELL
            
            if(S_host1_mat(row0, col0) > 0 || S_LD(row0, col0) > 0){  //IF SUSCEPTIBLE HOST IS AVAILABLE (UMCA OR LIDE)
              
              //WHAT IS THE PROBABILITY THAT A SPORE HITS ANY SUSCEPTIBLE HOST?
              PropS = double(S_host1_mat(row0, col0) + S_LD(row0, col0)) / N_LVE(row0, col0);
              
              double U = R::runif(0,1);		
              
              //if U < ProbS then one of the susceptible trees will get hit
              if (U < PropS){
                
                //WHICH SUSCEPTIBLE TREE GETS HIT? CALCULATE PROBABILITY WEIGHTS
                double Prob_UM = double (S_host1_mat(row0, col0)) / (S_host1_mat(row0, col0) + S_LD(row0, col0)); 
                double Prob_LD = double (S_LD(row0, col0)) / (S_host1_mat(row0, col0) + S_LD(row0, col0));
                
                //sample which of the three hosts will be hit given probability weights
                IntegerVector sv = sample(seq_len(2), 1, false, 
                                          NumericVector::create(Prob_UM, Prob_LD));
                
                //WHAT IS THE PROBABILITY THAT A SPORE TURNS INTO AN INFECTION, GIVEN THAT A SUSCEPTIBLE TREE HAS BEEN HIT?
                //This depends on both the local weather AND the host competency score (relative to UMCA and OAKS)
                int s = sv[0];
                if (s == 1){
                  double ProbINF = Prob_UM * W(row0, col0);
                  if (U < ProbINF){
                    I_host1_mat(row0, col0) = I_host1_mat(row0, col0) + 1; //update infected UMCA
                    S_host1_mat(row0, col0) = S_host1_mat(row0, col0) - 1; //update susceptible UMCA
                  }
                }else{
                  double ProbINF = Prob_LD * W(row0, col0);
                  if (U < ProbINF){
                    I_LD(row0, col0) = I_LD(row0, col0) + 1; //update infected LIDE
                    S_LD(row0, col0) = S_LD(row0, col0) - 1; //update susceptible LIDE                    
                  }                
                }  
              }//END IF U < PROB
            }//ENF IF S > 0  
          } //END IF DISTANCE CHECK
          
        }//END LOOP OVER ALL SPORES IN CURRENT CELL GRID     
      }//END IF SPORES IN CURRENT CELL
    }   
  }//END LOOP OVER ALL GRID CELLS
  
  //return List::create(Named("I")=I, Named("S")=S);
  return List::create(
    _["S_host1_mat"] = S_host1_mat, 
    _["I_host1_mat"] = I_host1_mat,
    _["S_host2_mat"] = S_host2_mat, 
    _["I_host2_mat"] = I_host2_mat,
    _["S_LD"] = S_LD, 
    _["I_LD"] = I_LD
  );
  
} //END OF FUNCTION		