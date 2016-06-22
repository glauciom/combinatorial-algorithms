package permutation;

/**
 * @author Glaucio Melo
 * Classe reponsavel pelo processamento do algoritmo SerialOffset.
 */
public class BlindOffset {
	
	// Campos da Classe SerialOffset.
	
	private int[] fatorial;
	private int[] offset;
	private int i,n;
	
	/**
	 * Construtor da Classe SerialOffset.
	 * @param fatorial vetor contendo os fatoriais dos numeros.
	 * @param n tamanho do vetor de deslocamento.
	 */
	public BlindOffset(int[] fatorial, int n) {
		this.fatorial = fatorial;
		this.n = n;
		this.offset = new int[n];
	}
	
	/**
	 * Metodo que processa o vetor de deslocamento.
	 * @param s Serial da permutacao.
	 */
	public void blindOffsetAlgorithm(int s) {
		offset = blindAlgorithm(s);
	}
	
	/**
	 * Metodo que captura o vetor de deslocamento.
	 * @return int[] Vetor de deslocamento.
	 */
	public int[] getOffset() {
		return offset;
	}
	
	// Metodo nucleo do algoritmo.
	
	private int[] blindAlgorithm(int s) {
		for(i = 1; i <= n; i++)
		 offset[i-1] = ((int) Math.floor((s - 1) / fatorial[i]) % 2 == 1 ? i - (int) Math.floor(((s-1) % fatorial[i]) / fatorial[i-1]) : 
		 (int) Math.floor(((s-1) % fatorial[i]) / fatorial[i-1])); 
		 
		 /*if((int) Math.floor((s - 1) / fatorial[i]) % 2 == 1)
		  offset[i-1] = i - (int) Math.floor(((s-1) % fatorial[i]) / fatorial[i-1]);
		 else
		  offset[i-1] = (int) Math.floor(((s-1) % fatorial[i]) / fatorial[i-1]);*/
	return offset;
	}
	
	/**
	 * Metodo que retorna o vetor de deslocamento em forma de String.
	 * @return String String do vetor de deslocamento.
	 */
	public String getOutPut() {
		StringBuffer k1 = new StringBuffer();
		for(i = 0; i < n; i++)
		 k1.append(offset[i] + " ");
	return k1.toString();
	}
}

