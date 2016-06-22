package subset;

import exceptions.OutOfRangeException;

/**
 * @author Glaucio Melo.
 * Classe responsavel pelo processamento do algoritmo NextSubset.
 */
public class NextSubset {
	
	// Campos da Classe NextSubset.
	
	private int i,k,t,j,n;
	private int subset[];
	private boolean flag = false;

	/**
	 * Construtor de NextSub.
	 * @param n Tamanho do vetor do conjunto.
	 */
	public NextSubset(int n) {
		this.n = n;
		this.subset = new int[n];
	}
	
	// Metodo acessorio da classe.
	
	private void calc() {
	 subset[j] = 1 - subset[j];
	 k = k + 2 * subset[j] -1;		
	}
	
	// Calcula o valor subset ser modificado quando par.
	
	private int[] calcEven() {
	 if (flag)
	  return null;
	 calc();
	 if (k == subset[n-1])
	  flag = true;
	 return subset;
	 	
	}
	
	// Calcula o valor subset ser modificado quando impar;
	
	private int[] calcOdd() {
	 if (flag)
	  return null;
	 do j++; while(subset[j-1] != 1);
	 calc();
	 if (k == subset[n-1])
	  flag = true;
	 return subset;
	}
	// Metodo nucleo da classe
	
	private int[] algorithm() {
	 t = k % 2;
	 j = 0;
	 return subset = (t == 0 ? calcEven() : calcOdd());
	}
	
	// Calcula o proximo conjunto, tratando a excecao de limites de serial.
	
	private void nextSubset()  throws OutOfRangeException {
		 subset = algorithm();
		 if (subset == null)
		  throw new OutOfRangeException();
	}
	
	/**
	 * Calcula o proximo conjunto.
	 */
	public void nextSubsetAlgorithm() {
	 try {
	  nextSubset();
	 } catch(OutOfRangeException error) {
		error.printStackTrace();
	   }
	}
	
	/**
	 * Captura o conjunto atual.
	 * @return int[] Vetor do conjunto.
	 */
	public int[] getSubset() {
		return subset;
	}
	
	/**
	 * Retorna o conjunto equivalente ao serial passado como parametro.
	 * @param serial Serial do conjunto
	 * @return int[] Vetor do conjunto.
	 */
	public int[] getSerialSubset(int serial) {
		this.subset = new int[n];
		for(int g = 1; g < serial; g++)
		 nextSubsetAlgorithm();
	return subset;
	}
	
	/**
	 * Captura o vetor de conjunto, em forma de String.
	 * @return String Vetor transformado em String.
	 */
	public String getOutPut() {
	 StringBuffer k1 = new StringBuffer();
	 for(int h = 0; h < n; h++)
	  k1.append(subset[h] + " ");
	 return k1.toString();
	}
	
	/**
	 * Metodo que indica se o conjunto atual e o ultimo conjunto da lista.
	 * @return boolean Verdadeiro caso seja o ultimo; Falso, do contrario. 
	 */
	public boolean isLastSubset() {
		return flag;
	}
	/**
	 * Metodo main (para testes).
	 * @param args String de entrada.
	 */
	public static void main(String args[]) {
		NextSubset test = new NextSubset(5);
		System.out.println("1- "+test.getOutPut());
		int count = 2;
		while (!test.isLastSubset()) {
		 test.nextSubsetAlgorithm();
		 System.out.println(count + "- "+test.getOutPut());
		 count++;
	   }
	}
}
