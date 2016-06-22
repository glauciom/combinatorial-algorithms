/*
 * Created on 21/06/2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package biginteger;

import java.math.BigInteger;

/**
 * @author glaucio
 *
 * To change the template for this generated type comment go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
public class UnserialCompositionMethod {
	
	private int[] composition;
	private int n, k;
	private BigInteger serial, max;
	
	
	public UnserialCompositionMethod(int[] composition) {
		this.composition = composition;
	}
	
	/**
	 * Prepara os atributos iniciais.
	 * @param composition vetor de composição dado como entrada.
	 */
	private void prepareAttributes(int[] composition) {
		this.n = this.k = 0;
		this.k = composition.length;
		for (int i = 0; i < composition.length; i++) {
			n += composition[i];
		}
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
	private BigInteger getBinomialElements(int r, int s) {
		if ((r - s) < s)
			return new BigInteger(factorial(r, s)).divide(new BigInteger(factorial(r - s, 1)));
		return new BigInteger(factorial(r, r - s)).divide(new BigInteger(factorial(s, 1)));
	}
	
	/**
	 * Algoritmo nucleo da classe.
	 * @param composition Vetor de composição.
	 * @return Serial correspondende a esse vetor de composição.
	 */
	public String unserialCompositionAlgorithm(int[] composition) {
		prepareAttributes(composition);
		int x = n + k - 2;
		int y = n;
		serial = BigInteger.ONE;
	/*	System.out.println("n = "+n);
		System.out.println("k = "+k);*/
		for (int i = composition.length-1; i >= 1; i--) {
			for (int j = 0; j < composition[i]; j++) 
					serial = serial.add(getBinomialElements(x-j-((k-1)-i),y-j));
			x = x - composition[i];
			y = y - composition[i];
		}
		return serial.toString();
	}
	
	public String toString() {
		return ""+serial;
	}
	
	/**
	 * @return
	 */
	public String getSerial() {
		return serial.toString();
	}
	
	/**
	 * Captura o número total de composições de um inteiro n em k partes. 
	 * @return int
	 */
	public String getNumberOfCompositions() {
		return getBinomialElements(n+k-1,n).toString();
	}
	
	public void calculateMax() {
		if (max == null) {
			max = new BigInteger(getNumberOfCompositions());
		}
	}
	
	public String getMax() {
		return max.toString();
	}
	
	public static void main(String[] args) {		
		NextComposition next = new NextComposition(1018,20);
		int[] comp = next.getComposition();
		int i = 1;
		UnserialCompositionMethod test = new UnserialCompositionMethod(comp);
		while (!next.isLastComposition()) {
			System.out.println(test.unserialCompositionAlgorithm(comp));
			next.nextCompositionAlgorithm();
			comp = next.getComposition();
		}
	}
	/**
	 * @return
	 */
	public int getK() {
		return k;
	}

	/**
	 * @return
	 */
	public int getN() {
		return n;
	}

}
