package breeze.plot


/**
 * Represents a "function" T that knows its (finite) domain,
 * and can also be applied. Typeclass.
 * @author dlwh
 */
trait DomainFunction1D[T,K,+V] {
  def domain(t: T):IndexedSeq[K]
  def apply(t: T, k: K):V
}

object DomainFunction1D {
  implicit def arrIsDomainFunction[V]:DomainFunction1D[Array[V],Int,V] = {
    new DomainFunction1D[Array[V], Int, V] {
      def domain(t: Array[V]): IndexedSeq[Int] = 0 until t.length

      def apply(t: Array[V], k: Int): V = t(k)
    }
  }


  implicit def seqIsDomainFunction[T,V](implicit ev: T<:<collection.Seq[V]):DomainFunction1D[T,Int,V] = {
    new DomainFunction1D[T,Int,V] {
      def domain(t: T): IndexedSeq[Int] = (0 until t.length)

      def apply(t: T, k: Int): V = {
        t(k)
      }
    }

  }


  implicit def mapIsDomainFunction[T,K,V](implicit ev: T<:<collection.Map[K, V]):DomainFunction1D[T,K,V] = {
    new DomainFunction1D[T,K,V] {
      def domain(t: T): IndexedSeq[K] = t.keySet.toIndexedSeq

      def apply(t: T, k: K): V = {
        t(k)
      }
    }

  }


}


trait DomainFunction2D[T,K,+V] {
  def cols(t : T) : Int
  def rows(t : T) : Int
  def domain(t: T):IndexedSeq[(K,K)]
  def apply(t: T, k: (K, K)):V

  def valuesIterator(t : T) : Iterator[V] = domain(t).map(ij => apply(t, ij)).toIterator
}

object DomainFunction2D {

  implicit def _2dseqIsDomainFunction[T, V](implicit ev : T<:<collection.Seq[collection.Seq[V]]):DomainFunction2D[T, Int, V] = {
    new DomainFunction2D[T,Int, V] {
      def cols(t : T) = t.headOption.map(_.length).getOrElse(0)
      def rows(t : T) = t.length
      def domain(t : T) : IndexedSeq[(Int, Int)] = {
        for (i <- 0 until rows(t) ; j <- 0 until cols(t)) yield (i,j)
      }
      def apply(t : T, ij : (Int, Int)) = {
        val (i, j) = ij
        t(i)(j)
      }
    }
  }

  implicit def _2dArrayIsDomainFunction[V] :DomainFunction2D[Array[Array[V]], Int, V] = {
    new DomainFunction2D[Array[Array[V]],Int, V] {
      def cols(t : Array[Array[V]]) = t.headOption.map(_.length).getOrElse(0)
      def rows(t : Array[Array[V]]) = t.length
      def domain(t : Array[Array[V]]) : IndexedSeq[(Int, Int)] = {
        for (i <- 0 until rows(t) ; j <- 0 until cols(t)) yield (i,j)
      }
      def apply(t : Array[Array[V]], ij : (Int, Int)) = {
        val (i, j) = ij
        t(i)(j)
      }
    }
  }

}
