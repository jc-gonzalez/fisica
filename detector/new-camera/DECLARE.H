#define DECLARE_GET(T,F,V)	inline T F(void) { return( V ); }
#define DECLARE_SET(T,F,V)	inline void F(T x) { V = x; }

