/*
 * Created on 17/10/2004
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
package kNSubset;

import java.math.BigInteger;

/**
 * @author GLAUCIO
 *
 * TODO To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
public class UnSerialKNSubsetMethod {

	private int n, k,x,y,index;
	private BigInteger serial;
	private int[] subset;
	
	
	public UnSerialKNSubsetMethod(int n) {
		this.n = n;
	}
	
	
	//Calcula o fatorial de um numero.
	private String factorial(int r, int op) {
		BigInteger aux = BigInteger.ONE;
		for (int t = r; t > op; t--) {
			aux = aux.multiply(BigInteger.valueOf((long) t));
		}
		return aux.toString();
	}
	
	
    //Calcula o binomial, com otimizações quanto à multiplicação. 
	public BigInteger getBinomialElements(int r, int s) {
		if ((r - s) < s)
			return new BigInteger(factorial(r, s)).divide(new BigInteger(factorial(r - s, 1)));
		return new BigInteger(factorial(r, r - s)).divide(new BigInteger(factorial(s, 1)));
	}
	
	public BigInteger unserialKNSubsetAlgorithm(int[] subset) {
		this.subset = subset;
		this.k = subset.length;
		x = n;
		y = k-1;
		serial = BigInteger.ONE;
		index = 0;
		serial = element(subset);
		return serial;
	}
	
	public BigInteger element(int[] subset) {
		for(int i = 0; i < subset.length; i++) {
			for(int j = 1; j < subset[i] - index; j++)
				serial = serial.add(getBinomialElements(x-j,y));
			x = x - (subset[i] - index);
			y = y - 1;
			index = subset[i];
		}
		return serial;
	}
	
	public String toString() {
		return serial.toString();
	}
	
	public static void main(String[] args) {
		int n = 60;
		int k = 6;
		SerialKNSubsetMethod test = new SerialKNSubsetMethod(n,k);
		int k1 = test.getBinomialElements(n,k).intValue();
		System.out.println(k1);
		UnSerialKNSubsetMethod test1 = new UnSerialKNSubsetMethod(n);
		int[] res = {55, 56, 57, 58, 59, 60};
		test1.unserialKNSubsetAlgorithm(res);
		System.out.println(test1);
		
		/*for (int i = 1; i <= k1; i++) {
			test.serialKNSubsetAlgorithm(i+"");
			test1.unserialKNSubsetAlgorithm(test.getSubset());
			System.out.println(test1);
		}*/
	}
}
