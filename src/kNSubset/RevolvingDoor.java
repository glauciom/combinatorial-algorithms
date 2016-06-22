package kNSubset;

/**
 * @author Glaucio Melo.
 * Classe nesponsavel pelo processamento do algoritmo NXKSRD.
 */
public class RevolvingDoor {

	// Campos da classe RevolvingDoor.
	private int k,n,m;
	private int[] subset;
	private boolean flag;
	
	/**
	 * Construtor de RevolvingDoor
	 * @param n Tamanho do vetor de conjunto total.
	 * @param k Tamanho do vetor de sobconjunto.
	 */
	public RevolvingDoor(int n, int k) {
		this.n = n;
		this.k = k;
		flag = false;
		init();
	}
	
	// Inicializa o vetor de sobconjunto.
	private void init() {
	 subset = new int[k];
	 for(int h = 0; h < k; h++)
	  subset[h] = h+1;
	}
	
	// Metodo auxiliar, que faz o decremento do predecessor.
	private void predecessor(int j) {
	 subset[j]--;
	 if (j != 0)
	  subset[j-1] = j;
	}

	// Metodo auxiliar, que faz o incremento do sucessor.
	private void sucessor(int j) {
	 subset[j]++;
	 if (j != 0)
	  subset[j-1] = subset[j] -1;
	}
	
	// Metodo nucleo da classe RevolvingDoor.
	
	private int[] nxksrd(int j, boolean logical) {
	 if (flag)
	  return null;
	 if( j > k-1) {
	  subset[k-1] = k;
	  flag = true;
	 } else {
	 if (logical)
	  if(subset[j] == j+1)
		nxksrd(j+1,false);
	   else
		predecessor(j);
	 else {
	  if(j == k-1)
	   m = n;
	  else
	   m = subset[j+1]-1;
	  if(m == subset[j])
	   nxksrd(j+1,true);
	   else
		sucessor(j);
	 }
	 }
	 return subset;
	}

	/**
	 * Captura o vetor de conjunto, em forma de String.
	 * @return String Vetor transformado em String.
	 */
	public String getOutPut() {
	 StringBuffer k1 = new StringBuffer();
	 for(int h = 0; h < k; h++)
	  k1.append(subset[h] + " ");
	 return k1.toString();
	}
	
	/**
	 * Invoca o algoritmo nucleo da classe.
	 */
	public void revolvingDoorAlgorithm() {
		subset = nxksrd(0,k % 2 == 0);
	}
	
	/**
	 * Captura o vetor do subconjunto.
	 * @return int[] Vetor do sunconjunto.
	 */
	public int[] getSubset() {
		return subset;
	}
	
	/**
	 * Captura subset variavel logica que indica se o vetor de sobconjunto atual e o
	 * ultimo da lista.
	 * @return boolean True se for o ultimo da lista; falso, do contrario.
	 */
	public boolean isLastElement() {
		return flag;
	}	
		
	/**
	 * Atualiza o valor do subconjunto, dado um serial especifico.
	 * @param serial Numero inteiro correpondente a numeracao do subconjunto da lista.
	 */ 
	public void serialRevolvingDoor(int serial) {
		init();
		for(int g = 1; g < serial; g++) {
		 revolvingDoorAlgorithm();
		}
	}
	
	/**
	 * Metodo main (para testes).
	 * @param args String de entrada do prompt de comando.
	 */
	public static void main(String[] args) {
		RevolvingDoor test = new RevolvingDoor(8,5);
		while(!test.isLastElement()) {
			System.out.println(test.getOutPut());
			test.revolvingDoorAlgorithm();
		}
	}
}
