/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

/*
Student's T distribution                

References:

Abromowitz and Stegun, 1970 Ninth Edition
Handbook of Mathematical Functions with Formulas, Graphs, and
Mathematical Tables.
Page 948, Article 26.7 Student's t-Distribution: Series Expansions:
Formulas 26.7.3 and 26.7.4.

Explanations:

studentT( double t, int nu) returns the prob that the
distribution lies between -t and t.

For a double sided test the 95% confidence interval means 2.5% above the
upper limit and 2.5% below the lower limit. The nomenclature used here 
labels this as the 95% confidence limit. Wikipedia and Wolfram label it
as 97.5% in their tables.

http://mathworld.wolfram.com/Studentst-Distribution.html
http://en.wikipedia.org/wiki/Student's_t-distribution
*/

package JSim.data;
import java.lang.Object;
import java.lang.Math;
import JSim.util.*;
public class StudTDist {
        public int nu;
        // constructor
        public StudTDist (int nu) throws Xcept{ 
            this.nu = nu;
            if(nu<1) throw new Xcept(
        "Number of data points with non-zero weights minus number of parameters must be greater than zero." );
        }
/*
       Return distance,t, given degrees of freedom and desired probability
     such that the integral from -t to t of the Student's T
     distribution equals the desired probability with the given degrees
     of freedom.
     Iterative solution until difference between desired probability
     and calculated probability is less than 0.000001.
*/
        public double tValue(double prob) throws Xcept{
            double t = 1.0;
            if ((prob<1e-5)||(prob>0.99999)|| Double.isNaN(prob)) throw new Xcept(
            "Probability for Student's T test not between 0 and 1");

            double eps=0.001;
            double ft;
            ft=studentT(t);
            while (0.1e-5<Math.abs(ft-prob) ) {
                t=t+2.0*(prob-ft)*eps/(studentT(t+eps)-studentT(t-eps));
                ft = studentT(t);
            }
            return t;
        }

        // Return probability given t and nu 
        private double studentT( double t) {
            if(nu % 2 ==0) return feven(t);
            else if(nu==1) return (0.2e1*Math.atan(t)/Math.PI); 
            else return (fodd(t));
        }
        // nu (degrees of freedom) is even
        private double feven( double t) {
            double theta;
            double s;
            double c;
            double sum;
            double term;
            theta = (double)(Math.atan( t/Math.sqrt(nu)));
            s = Math.sin(theta);
            c = Math.cos(theta);
            sum=1.0;
            term=1.0;
            for ( int i=1; i<=nu-2; i +=2) { 
                term = term*i*c*c/(i+1) ;
                sum = sum + term;
            }
            return (s*sum);
        }

        // nu (degrees of freedom is odd
        private double fodd( double t) {
            double theta;
            double s;
            double c;
            double sum;
            double term;
            theta = (double)(Math.atan( t/Math.sqrt(nu)));
            s = Math.sin(theta);
            c = Math.cos(theta);
            sum = c;
            term = c;
            for ( int i=2; i<=nu-2; i +=2) { 
                term = term*i*c*c/(i+1) ;
                sum = sum + term;
            }
            return ( (0.2e1/Math.PI)*(theta+s*sum) );
        }


/*
Test Student's T distribution
*/
         // mainline
         public static void main(String[] args) throws Xcept {
             double prob=0.90;
             for (int n=1; n<=6; n++) {
                 int nux=n;
                 if(n==6) nux=10000;
                 StudTDist answer = new StudTDist(nux);
                 double ans = answer.tValue(prob);
                 System.out.println(nux); System.out.println(ans);
             }
          }
}
