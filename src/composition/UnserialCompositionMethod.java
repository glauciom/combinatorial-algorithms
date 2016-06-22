/*
 * Created on 21/06/2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package composition;

/**
 * @author glaucio
 *
 * To change the template for this generated type comment go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
public class UnserialCompositionMethod {
	
	private int[] composition;
	private int n, k;
	private long serial;
	
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
	
	//	Calcula o fatorial de um numero.
	  private long factorial(long r, long op) {
		  long aux = 1;
		  for (long t = r; t > op; t--)
			  aux *= t;
		  return aux;
	  }
	
	
	  //Calcula o binomial, com otimizações quanto à multiplicação. 
	  private long getBinomialElements(long r, long s) {
		  if ((r - s) < s)
			  return (factorial(r, s) / factorial(r - s, 1));
		  return (factorial(r, r - s) / factorial(s, 1));
	  }
	
	/**
	 * Algoritmo nucleo da classe.
	 * @param composition Vetor de composição.
	 * @return Serial correspondende a esse vetor de composição.
	 */
	public long unserialCompositionAlgorithm(int[] composition) {
		prepareAttributes(composition);
		int x = n + k - 2;
		int y = n;
		serial = 1;
	/*	System.out.println("n = "+n);
		System.out.println("k = "+k);*/
		for (int i = composition.length-1; i >= 1; i--) {
			for (int j = 0; j < composition[i]; j++) 
					serial += (long) getBinomialElements(x-j-((k-1)-i),y-j);
			x = x - composition[i];
			y = y - composition[i];
		}
		return serial;
	}
	
	public String toString() {
		return ""+serial;
	}
	
	/**
	 * @return
	 */
	public long getSerial() {
		return serial;
	}
	
	/**
	 * Captura o número total de composições de um inteiro n em k partes. 
	 * @return int
	 */
	public long getNumberOfCompositions() {
		return getBinomialElements(n+k-1,n);
	}
	
	public static void main(String[] args) {		
		NextComposition next = new NextComposition(7,5);
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
