package composition;


/**
 * Esta classe � respons�vel pelo processamento do algoritmo de composi��o
 * serial, obtido atrav�s de an�lise ao algoritmo de pr�xima composi��o.
 * @author Glaucio
 */

public class SerialComposition2 {

    //Vetor de composi��o.
	private int composition[];
	
	//Valor a ser composto e o numero de partes em que ele vai ser composto.
	private int n, k;
	
	//Serial de entrada para o algoritmo.
	private long serial;
	
	//Campos auxiliares ao m�todo.
	private long  aproximation, x, y;
	
	private int i,j,g, complement;

	
	/**
	 * Construtor para a classe SerialCompositionMethod
	 * @param n Numero que ir� ser composto.
	 * @param k Numero de partes em que n ser� composto.
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
	
	
    //Calcula o binomial, com otimiza��es quanto � multiplica��o. 
	private long getBinomialElements(long r, long s) {
		if ((r - s) < s)
			return (factorial(r, s) / factorial(r - s, 1));
		return (factorial(r, r - s) / factorial(s, 1));
	}
	
	
	//M�todo nucleo que captura o valor do elemento do vetor de composi��o. 
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
	
	
	//M�todo nucleo de invoca��o do algoritmo.
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
	 * M�todo de invoca��o do algoritmo de composi��o serial.
	 * @param serial Serial passado como par�metro.
	 */
	public void serialComposition(long serial) {
		composition = serialCompositionAlgorithm(serial-1);
	}

	
	/**
	 * M�todo sobreposto que retorna como sa�da o vetor de composi��o, sob forma
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
	 * Captura o n�mero total de composi��es de um inteiro n em k partes. 
	 * @return int
	 */
	public long getNumberOfCompositions() {
		return getBinomialElements(n+k-1,n);
	}
	
	
	/**
	 * M�todo main (para testes)
	 * @param args Par�metros de entrada ao m�todo.
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