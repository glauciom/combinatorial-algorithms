package permutation;


/**
 * @author Glaucio Melo
 * Classe que retorna um vetor permutado, dado o seu vetor de deslocamento.
 */
public class BlindPermutation {
	
	//Campos de SerialPermutation.
	private int[] elements;
	private int[] result;
	private int[] offset;
	private int i,j,n;
	
	/**
	 * Construtor de SerialPermutation.
	 * @param offset Vetor de Deslocamento.
	 * @param n tamanho do vetor de elementos.
	 */
	
	public BlindPermutation(int[] offset,int n) {
		this.offset = offset;
		this.n = n;
		init();	
	} 
	
	/**
	 * Metodo de invocacao do algoritmo.
	 */
	public void blindPermutationAlgorithm() {
		result = blindAlgorithm();
	}
	
	
	/**
	 * Metodo que retorna o vetor de permutacao sob forma de String.
	 * @return String Vetor em forma de String.
	 */
	
	public String getOutPut() {
	 StringBuffer k1 = new StringBuffer();
	 for(i = 0; i < n; i++)
	  k1.append(result[i] + " ");
	return k1.toString();
	}
	

	// Metodo nucleo do algoritmo.
	
	private int push() {
	 for(j = (n-1)-i; j < (n - 1) - offset[i-1]; j++)
	  elements[j] = elements[j+1];
	return i+1;
	}
	
	// Inicializando o vetor elements.
	
	private void init() {
	 this.elements = new int[n];
	 this.result = new int[n];
	 for(i = 0; i < n; i++)
	 	elements[i] = i+1;
	}
	  
	// Metodo nucleo do algoritmo.
	
	private int[] blindAlgorithm() {
	 elements[n-1] = 1;
	 for(i = 1; i < n; i++)
	  elements[n-1-offset[i-1]] = elements[n-1-offset[i-1]] == 0 ? i+1 : push();
	 for(i = 0; i < n; i++)
	  result[elements[i]-1] = i;
	return result;
	}
		
	/**
	 * Captura o vetor de Elementos da permutacao.
	 * @return int[] Vetor de Elementos.
	 */
	public int[] getPermutation() {
		return result;
	}
	
	/**
	 * Captura o vetor de deslocamento.
	 * @return int[] Vetor de deslocamento.
	 */
	public int[] getOffset() {
		return offset;
	}
}
