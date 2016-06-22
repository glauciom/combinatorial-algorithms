package composition;


/**
 * Esta classe é responsável pelo processamento do algoritmo de composição
 * serial, obtido através de análise ao algoritmo de próxima composição.
 * @author Glaucio
 */

public class SerialComposition2 {

    //Vetor de composição.
	private int composition[];
	
	//Valor a ser composto e o numero de partes em que ele vai ser composto.
	private int n, k;
	
	//Serial de entrada para o algoritmo.
	private long serial;
	
	//Campos auxiliares ao método.
	private long  aproximation, x, y;
	
	private int i,j,g, complement;

	
	/**
	 * Construtor para a classe SerialCompositionMethod
	 * @param n Numero que irá ser composto.
	 * @param k Numero de partes em que n será composto.
	 */
	public SerialComposition2(int n, int k) {
		this.n = n;
		this.k = k;
		composition = new int[k];
	}
	
	
	//Calcula o fatorial de um numero.
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
	
	
	//Método nucleo que captura o valor do elemento do vetor de composição. 
	private int getElement(long serial) {
		for (j = 0; j < n; j++)
			if ((aproximation + getBinomialElements(x - i - j, y - j))
				<= serial)
				aproximation += getBinomialElements(x - i - j, y - j);
			else {
				x = x - j;
				y = y - j;
				return j;
			}
		return n;
	}
	
	
	//Método nucleo de invocação do algoritmo.
	private int[] serialCompositionAlgorithm(long serial) {
		complement = 0;
		aproximation = 0;
		x = n + k - 2;
		y = n;
		for (i = 0; i < k - 1; i++) {
			g = getElement(serial);
			composition[k - 1 - i] = g;
			if (composition[(int)k - 1 - i] != 0)
				complement += composition[k - 1 - i];
		}
		composition[0] = n - complement;
		return composition;
	}
	
	
	/**
	 * Método de invocação do algoritmo de composição serial.
	 * @param serial Serial passado como parâmetro.
	 */
	public void serialComposition(long serial) {
		composition = serialCompositionAlgorithm(serial-1);
	}

	
	/**
	 * Método sobreposto que retorna como saída o vetor de composição, sob forma
	 * de String.
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		StringBuffer k1 = new StringBuffer();
		for (int i = 0; i < k; i++)
			k1.append(composition[i] + " ");
		return k1.toString();
	}
	
	/**
	 * Captura o número total de composições de um inteiro n em k partes. 
	 * @return int
	 */
	public long getNumberOfCompositions() {
		return getBinomialElements(n+k-1,n);
	}
	
	
	/**
	 * Método main (para testes)
	 * @param args Parâmetros de entrada ao método.
	 */
	public static void main(String[] args) {
		SerialComposition2 test = new SerialComposition2(7,5);
		for(int i = 1; i <= test.getNumberOfCompositions(); i++) {
	//	 Date r = new Date();
	//	 long t0 = r.getTime();
		 test.serialComposition(i);
		 System.out.println(i + "-> "+ test);
		}
	//    r = new Date();
	//    long t1 = r.getTime() - t0; 
	 //   System.out.println("tempo: "+ t1);
	}

}