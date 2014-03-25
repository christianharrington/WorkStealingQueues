package dk.itu.wsq.queue

class CircularArray[E: Manifest](val logSize: Int) {
  // Initial size = 2^logSize
  private val segment: Array[E] = new Array[E](1 << logSize)

  def size: Long = 1 << logSize

  def apply(index: Long): E = get(index)

  def update(index: Long, element: E) = put(index, element)

  def get(index: Long): E = {
    segment((index % size).toInt)
    // TODO: Improve this by using a bit mask
  }

  def put(index: Long, element: E): Unit = {
    segment((index % size).toInt) = element
    // TODO: Improve this by using a bit mask
  }

  def grow(bottom: Long, top: Long): CircularArray[E] = {
    val newArray = new CircularArray(logSize + 1)
    var i = top
    for (i <- top until bottom) {
      newArray(i) = get(i)
    }

    newArray
  }

  def shrinkNaive(bottom: Long, top: Long): CircularArray[E] = {
    val newArray = new CircularArray(logSize - 1)
    var i = top
    for (i <- top until bottom) {
      newArray(i) = get(i)
    }

    newArray    
  }
  
}

class ChaseLevQueue[E: Manifest] extends WorkStealingQueue[E] {
  import java.util.concurrent.atomic._
  
  // Never decremented. Assumed to never overflow.
  protected var top: AtomicLong = new AtomicLong(0)
  // Indicates where next element is pushed
  @volatile protected var bottom: Long = 0
  
  // Initial size is 2^logInitialSize
  protected val logInitialSize = 2
  @volatile protected var activeArray = new CircularArray[E](logInitialSize)

  final def push(element: E): Unit = {
    val b = bottom
    val t = top.get
    var arr = activeArray
    val size = b - t
    // If the array is too small, grow the array
    if (size >= arr.size - 1) {
      arr = arr.grow(b, t)
      activeArray = arr
    }

    arr(b) = element
    bottom = b + 1
  }

  final def steal(): Option[E] = {
    val t = top.get
    val b = bottom
    val arr = activeArray
    val size = b - t

    if (size <= 0) {
      None
    } else {
      val elem = arr(t) // Get top element

      if (top.compareAndSet(t, t + 1)) 
        Some(elem)
      else 
        None
    }
  }

  def take(): Option[E] = {
    var b = bottom
    val arr = activeArray
    b = b - 1
    bottom = b
    val t = top.get
    val size = b - t

    if (size < 0) {
      bottom = t
      None
    } else {
      val elem = arr(b) // Get bottom element

      if (size > 0) {
        Some(elem)
      } else {
        bottom = t + 1
        if (top.compareAndSet(t, t + 1)) 
          Some(elem) 
        else
          None
      }
    }
  }

  final def length = (bottom - top.get).toInt
}

trait ChaseLevShrinkingQueue[E] extends ChaseLevQueue[E] {
  protected def perhapsShrink(bottom: Long, top: Long): Unit

  override final def take(): Option[E] = {
    var b = bottom
    val arr = activeArray
    b = b - 1
    bottom = b
    val t = top.get
    val size = b - t

    if (size < 0) {
      bottom = t
      None
    } else {
      val elem = arr(b)

      if (size > 0) {
        perhapsShrink(b, t)
        Some(elem)
      } else {
        bottom = t + 1
        if (top.compareAndSet(t, t + 1)) 
          Some(elem) 
        else
          None
      }
    }    
  }
}

class ChaseLevNaiveShrinkingQueue[E: Manifest] extends ChaseLevShrinkingQueue[E] {
  private val shrinkingConstant = 4 // Should be ≥ 3

  protected final def perhapsShrink(bottom: Long, top: Long): Unit = {
    val arr = activeArray
    if ((bottom - top) < (arr.size / shrinkingConstant)) {
      val newArray = arr.shrinkNaive(bottom, top)
      activeArray = newArray
    }
  }
}
