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
public class SerialKNSubsetMethod {
	private int n, k,x,y,index;
	private BigInteger serial, aproximation;
	private int[] subset;
	
	public SerialKNSubsetMethod(int n, int k) {
		this.n = n;
		this.k = k;
		this.subset = new int[k];
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
	
	public int[] serialKNSubsetAlgorithm(String number) {
		x = n;
		y = k-1;
		serial = new BigInteger(number);
		aproximation = BigInteger.ZERO;
		index = 0;
		for(int i = 0; i < k; i++) {
			subset[i] = element(serial);
		}
		return subset;
	}
	
	public int element(BigInteger serial) {
		BigInteger aux = null;
		for(int j = 1; j <= x - y + 1; j++) {
			aux = aproximation.add(getBinomialElements(x-j,y));
			if (aux.compareTo(serial) < 0)
				aproximation = aux;
			else {
				x = x - j;
				y = y - 1;
				index = index + j;
				return index;
			}
		}
		return index;
	}
	
	public String toString() {
		StringBuffer k1 = new StringBuffer();
		for (int i = 0; i < k; i++) {
			k1.append(subset[i] + " ");
		}
		return k1.toString();
	}
	
	public static void main(String[] args) {
		SerialKNSubsetMethod test = new SerialKNSubsetMethod(8,5);
		LexicographicNextKSubset t = new LexicographicNextKSubset(8,5);
		int k = test.getBinomialElements(8,5).intValue();
		for (int i = 1; i < k; i++) {
			test.serialKNSubsetAlgorithm(i+"");
			System.out.println(i+" = "+test);
		}

		//	for (int i = 1; i <= test.getBinomialElements(7,4).intValue(); i++) {
	/*		test.serialKNSubsetAlgorithm(test.getBinomialElements(60,6).toString());
			System.out.println(test);		*/	
	//	}

	}
	
	/**
	 * @return Returns the subset.
	 */
	public int[] getSubset() {
		return subset;
	}
	/**
	 * @param subset The subset to set.
	 */
	public void setSubset(int[] subset) {
		this.subset = subset;
	}
}
