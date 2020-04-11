#ifndef __COLOUR_H
#define __COLOUR_H

#include "Math3D.h"

class ColourRGB {
public:
	ColourRGB() {
	};

	ColourRGB(SCALAR intensity) {
		r = intensity;
		g = intensity;
		b = intensity;
	};

	ColourRGB(SCALAR _r, SCALAR _g, SCALAR _b) {
		r = _r;
		g = _g;
		b = _b;
	};

	void operator+= ( const ColourRGB &c ) {
		r += c.r;
		g += c.g;
		b += c.b;
	}

	SCALAR r, g, b;
};

inline ColourRGB operator+(const ColourRGB &a, const ColourRGB &b) {
	return ColourRGB(a.r + b.r, a.g + b.g, a.b + b.b);
}

inline ColourRGB operator*(const ColourRGB &a, const ColourRGB &b) {
	return ColourRGB(a.r * b.r, a.g * b.g, a.b * b.b);
}

inline ColourRGB operator*(const ColourRGB &a, SCALAR i) {
	return ColourRGB(a.r * i, a.g * i, a.b * i);
}

class ColourRGBFT {
public:
	ColourRGBFT() {
	};

	ColourRGBFT(SCALAR intensity) {
		r = intensity;
		g = intensity;
		b = intensity;
		f = static_cast<SCALAR>(0);
		t = static_cast<SCALAR>(0);
	};

	ColourRGBFT(SCALAR _r, SCALAR _g, SCALAR _b) {
		r = _r;
		g = _g;
		b = _b;
		f = static_cast<SCALAR>(0);
		t = static_cast<SCALAR>(0);
	};

	ColourRGBFT(SCALAR _r, SCALAR _g, SCALAR _b, SCALAR _f, SCALAR _t) {
		r = _r;
		g = _g;
		b = _b;
		f = _f;
		t = _t;
	};

	ColourRGB makeRGB() {
		return ColourRGB(r, g, b);
	}
	SCALAR r, g, b, f, t;

	// Get vector value
	const SCALAR& operator[](int index) const {
	    return (&r)[index];
	}

	// Set vector value
	SCALAR& operator[](int index) {
	    return (&r)[index];
	}
};
#endif

