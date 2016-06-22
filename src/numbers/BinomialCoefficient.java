/*
 * Created on 18/03/2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package numbers;

/**
 * @author Glaucio
 *
 * To change the template for this generated type comment go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
public class BinomialCoefficient {

	//Calcula o fatorial de um numero.
	public static long factorial(long r, long op) {
		long aux = 1;
		for (long t = r; t > op; t--)
			aux *= t;
		return aux;
	}
	
	
	//Calcula o binomial, com otimizações quanto à multiplicação. 
	public static long getBinomialElements(long r, long s) {
		if ((r - s) < s)
			return (factorial(r, s) / factorial(r - s, 1));
		return (factorial(r, r - s) / factorial(s, 1));
	}
	
	public static void main(String[] args) {
		System.out.println(BinomialCoefficient.getBinomialElements(60,6));
		
	}
}
