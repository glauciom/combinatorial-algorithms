package biginteger;

import java.util.Date;


/**
 * @author Glaucio Melo
 * Classe responsavel pelo processamento do algoritmo de proxima composicao.
 */
public class NextComposition {

	// Campos da Classe NextComposition.
	private int n,k,h,t;
	private int composition[];
	private boolean flag = false;

	/**
	 * Construtor da classe NextComposition.
	 * @param n Numero para composicao.
	 * @param k Tamanho do vetor de partes da composicao.
	 */
	public NextComposition(int n, int k) { 
		this.n = n;
		this.k = k;
		init();
	}
	
	// Realiza a transformaxao no vetor que representa as partes da composicao.
	private int[] transform() {
	 if (t != 1)
	  h = -1;
	 h++;
	 t = composition[h];
	 composition[h] = 0;
	 composition[0] = t-1;
	 composition[h+1]++;
	 return composition;
	}
	
	//Metodo nucleo da classe.
	
	private int[] nexcom() {
	 return composition = (composition[k-1] == n ? null : transform());
	}
	
	//Inicializa o vetor das partes da composicao.
	
	private void init() {
	 composition = new int[k];
	 composition[0] = n;
	 t = n;
	 h = -1;
	 for(int i = 1; i < k; i++)
	  composition[i] = 0;
	}
	
	/**
	 * Metodo que indica se a composicao atual e a ultima da lista.
	 * @return boolean True, caso seja a ultima da lista; false, do contrario.
	 */
	public boolean isLastComposition() {
		return (composition == null);
	}

	/**
	 * Captura o vetor de partes da composicao, em forma de String.
	 * @return String Vetor transformado em String.
	 */
	public String getOutPut() {
	 StringBuffer k1 = new StringBuffer();
	 for(int g = 0; g < k; g++)
	  k1.append(composition[g] + " ");
	 return k1.toString();
	}
	
	/**
	 * Metodo de invocacao do algoritmo nucleo da classe NextComposition.
	 */
	public void nextCompositionAlgorithm() {
		composition = nexcom();
	}
	
	/**
	 * Captura o vetor de composicao atual.
	 * @return int[] Vetor representando a composicao atual.
	 */
	public int[] getComposition() {
		return composition;
	}
	
	/**
	 * Captura a composicao, dado um numero serial.
	 * @param serial Numero que representa uma determinada composicao.
	 */
	public void serialComposition(int serial) { 
		init();
		for(int i = 1; i < serial; i++)
		 nextCompositionAlgorithm();
	}

	/**
	 * Metodo main (para testes)
	 * @param args String de entrada no prompt de comando.
	 */
	public static void main(String[] args) {
	 NextComposition test = new NextComposition(7,5);
	 int i = 1;
	 Date r = new Date();
	 long t0 = r.getTime();
	 while(!test.isLastComposition()) {
		System.out.println(i+" -> "+test.getOutPut());
		i++;
	 	test.nextCompositionAlgorithm();
	 }
	 r = new Date();
	long t1 = r.getTime() - t0; 

	System.out.println("tempo: "+ t1);
	}
}
