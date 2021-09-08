
let jsr = 0x5EED;
let {PI} = Math;
function rand(){
  jsr^=(jsr<<17);
  jsr^=(jsr>>13);
  jsr^=(jsr<<5);
  return (jsr>>>0)/4294967295;
}

var PERLIN_YWRAPB = 4; var PERLIN_YWRAP = 1<<PERLIN_YWRAPB;
var PERLIN_ZWRAPB = 8; var PERLIN_ZWRAP = 1<<PERLIN_ZWRAPB;
var PERLIN_SIZE = 4095;
var perlin_octaves = 4;var perlin_amp_falloff = 0.5;
var scaled_cosine = function(i) {return 0.5*(1.0-Math.cos(i*PI));};
var perlin;
let noise = function(x,y,z) {
  y = y || 0; z = z || 0;
  if (perlin == null) {
    perlin = new Array(PERLIN_SIZE + 1);
    for (var i = 0; i < PERLIN_SIZE + 1; i++) {
      perlin[i] = rand();
    }
  }
  if (x<0) { x=-x; } if (y<0) { y=-y; } if (z<0) { z=-z; }
  var xi=Math.floor(x), yi=Math.floor(y), zi=Math.floor(z);
  var xf = x - xi; var yf = y - yi; var zf = z - zi;
  var rxf, ryf;
  var r=0; var ampl=0.5;
  var n1,n2,n3;
  for (var o=0; o<perlin_octaves; o++) {
    var of=xi+(yi<<PERLIN_YWRAPB)+(zi<<PERLIN_ZWRAPB);
    rxf = scaled_cosine(xf); ryf = scaled_cosine(yf);
    n1  = perlin[of&PERLIN_SIZE];
    n1 += rxf*(perlin[(of+1)&PERLIN_SIZE]-n1);
    n2  = perlin[(of+PERLIN_YWRAP)&PERLIN_SIZE];
    n2 += rxf*(perlin[(of+PERLIN_YWRAP+1)&PERLIN_SIZE]-n2);
    n1 += ryf*(n2-n1);
    of += PERLIN_ZWRAP;
    n2  = perlin[of&PERLIN_SIZE];
    n2 += rxf*(perlin[(of+1)&PERLIN_SIZE]-n2);
    n3  = perlin[(of+PERLIN_YWRAP)&PERLIN_SIZE];
    n3 += rxf*(perlin[(of+PERLIN_YWRAP+1)&PERLIN_SIZE]-n3);
    n2 += ryf*(n3-n2);
    n1 += scaled_cosine(zf)*(n2-n1);
    r += n1*ampl;
    ampl *= perlin_amp_falloff;
    xi<<=1; xf*=2; yi<<=1; yf*=2; zi<<=1; zf*=2;
    if (xf>=1.0) { xi++; xf--; }
    if (yf>=1.0) { yi++; yf--; }
    if (zf>=1.0) { zi++; zf--; }
  }
  return r;
};

function dist(x0,y0,x1,y1){
  return Math.hypot(x1-x0,y1-y0);
}
function lerp(a,b,t){
  return a * (1-t) + b * t;
}
function lerp2d(x0,y0,x1,y1,t){
  return [
    x0*(1-t) + x1*t,
    y0*(1-t) + y1*t,
  ]
}
function get_bbox(points){
  let xmin = Infinity;
  let ymin = Infinity;
  let xmax = -Infinity;
  let ymax = -Infinity
  for (let i = 0;i < points.length; i++){
    let [x,y] = points[i];
    xmin = Math.min(xmin,x);
    ymin = Math.min(ymin,y);
    xmax = Math.max(xmax,x);
    ymax = Math.max(ymax,y);
  }
  return {x:xmin,y:ymin,w:xmax-xmin,h:ymax-ymin};
}

function seg_isect(p0x, p0y, p1x, p1y, q0x, q0y, q1x, q1y, is_ray = false) {
  let d0x = p1x - p0x;
  let d0y = p1y - p0y;
  let d1x = q1x - q0x;
  let d1y = q1y - q0y;
  let vc = d0x * d1y - d0y * d1x;
  if (vc == 0) {
    return null;
  }
  let vcn = vc * vc;
  let q0x_p0x = q0x - p0x;
  let q0y_p0y = q0y - p0y;
  let vc_vcn = vc / vcn;
  let t = (q0x_p0x * d1y - q0y_p0y * d1x) * vc_vcn;
  let s = (q0x_p0x * d0y - q0y_p0y * d0x) * vc_vcn;
  if (0 <= t && (is_ray || t < 1) && 0 <= s && s < 1) {
    let ret = {t, s, side: null, other: null, xy: null};
    ret.xy = [p1x * t + p0x * (1 - t), p1y * t + p0y * (1 - t)];
    ret.side = pt_in_pl(p0x, p0y, p1x, p1y, q0x, q0y) < 0 ? 1 : -1;
    return ret;
  }
  return null;
}
function pt_in_pl(x, y, x0, y0, x1, y1) {
  let dx = x1 - x0;
  let dy = y1 - y0;
  let e = (x - x0) * dy - (y - y0) * dx;
  return e;
}

function poly_bridge(poly0,poly1){
  let dmin = Infinity;
  let imin = null;
  for (let i = 0; i < poly0.length; i++){
    for (let j = 0; j < poly1.length; j++){
      let [x0,y0] = poly0[i];
      let [x1,y1] = poly1[j];
      let dx = x0-x1;
      let dy = y0-y1;
      let d2 = dx*dx + dy*dy;
      if (d2 < dmin){
        dmin = d2;
        imin = [i,j];
      }
    }
  }
  let u = poly0.slice(0,imin[0]).concat(
    poly1.slice(imin[1])).concat(
      poly1.slice(0,imin[1])).concat(
        poly0.slice(imin[0]));
  return u;
}

function poly_union(poly0,poly1,self_isect=false){
  let verts0 = poly0.map(xy=>({xy, isects: [], isects_map: {}}));
  let verts1 = poly1.map(xy=>({xy, isects: [], isects_map: {}}));

  function pair_key() {
    return Array.from(arguments).join(',');
  }

  let has_isect = false;

  function build_vertices(poly,other,out,oout,idx){
    let n = poly.length;
    let m = other.length;
    if (self_isect){
      for (let i = 0; i < n; i++) {
        let id = pair_key(idx,i);
        let p = out[i];
        let i1 = (i + 1 + n) % n;
        let a = poly[i];
        let b = poly[i1];
        for (let j = 0; j < n; j++) {
          let jd = pair_key(idx,j);
          let j1 = (j + 1 + n) % n;
          if (i == j || i == j1 || i1 == j || i1 == j1) {
            continue;
          }
          let c = poly[j];
          let d = poly[j1];
          let xx;
          let ox = out[j].isects_map[id];
          if (ox) {
            xx = {
              t: ox.s,
              s: ox.t,
              xy: ox.xy,
              other: null,
              side: pt_in_pl(...a, ...b, ...c) < 0 ? 1 : -1
            };
          }else{
            xx = seg_isect(...a, ...b, ...c, ...d);
          }
          if (xx) {
            xx.other = j;
            xx.jump = false;
            p.isects.push(xx);
            p.isects_map[jd] = xx;
          }
        }
        
      }
    }

    for (let i = 0; i < n; i++) {
      let id = pair_key(idx,i);
      let p = out[i];
      let i1 = (i + 1 + n) % n;
      let a = poly[i];
      let b = poly[i1];
      for (let j = 0; j < m; j++) {
        let jd = pair_key(1-idx,j);
        let j1 = (j + 1 + m) % m;
        let c = other[j];
        let d = other[j1];
        let xx;

        let ox = oout[j].isects_map[id];
        if (ox) {
          xx = {
            t: ox.s,
            s: ox.t,
            xy: ox.xy,
            other: null,
            side: pt_in_pl(...a, ...b, ...c) < 0 ? 1 : -1
          };
        } else {
          xx = seg_isect(...a, ...b, ...c, ...d);
        }
        if (xx) {
          has_isect = true;
          xx.other = j;
          xx.jump = true;
          p.isects.push(xx);
          p.isects_map[jd] = xx;
        }
      }
      p.isects.sort((a2, b2) => a2.t - b2.t);
    }
  }
  build_vertices(poly0,poly1,verts0,verts1,0);
  build_vertices(poly1,poly0,verts1,verts0,1);
  
  if (!has_isect){
    if (!self_isect){
      return poly_bridge(poly0,poly1);
    }else{
      return poly_union(poly_bridge(poly0,poly1),[],true);
    }
  }


  let isect_mir = {};
  function mirror_isects(verts0,verts1,idx) {
    let n = verts0.length;
    for (let i = 0; i < n; i++) {
      for (let j = 0; j < verts0[i].isects.length; j++) {
        let id = pair_key(idx, i, j);
        let {jump} = verts0[i].isects[j];
        let jd = jump?(1-idx):idx;
        let k = verts0[i].isects[j].other;
        let z = (jump?verts1:verts0)[k].isects.findIndex((x) => (x.jump == jump && x.other == i));
        isect_mir[id] = [jd, k, z];
      }
    }
  }
  mirror_isects(verts0,verts1,0);
  mirror_isects(verts1,verts0,1);

  // console.log(verts0,verts1)

  function trace_outline(idx, i0, j0, dir) {
    let zero = null;
    let out = [];
    function trace_from(idx, i0, j0, dir) {
      if (zero == null) {
        zero = [idx, i0, j0];
      } else if (idx == zero[0] && i0 == zero[1] && j0 == zero[2]) {
        return true;
      }
      let verts = idx?verts1:verts0;
      let n = verts.length;
      let p = verts[i0];
      let i1 = (i0 + dir + n) % n;
      if (j0 == -1) {
        out.push(p.xy);
        if (dir < 0) {
          return trace_from(idx,i1, verts[i1].isects.length - 1, dir);
        } else if (!verts[i0].isects.length) {
          return trace_from(idx, i1, -1, dir, [i0, j0]);
        } else {
          return trace_from(idx, i0, 0, dir, [i0, j0]);
        }
      } else if (j0 >= p.isects.length) {
        return trace_from(idx, i1, -1, dir, [i0, j0]);
      } else {
        let id = pair_key(idx, i0, j0);
        out.push(p.isects[j0].xy);

        let q = p.isects[j0];
        let [jdx, k, z] = isect_mir[id];
        let params;
        if (q.side * dir < 0) {
          params = [jdx, k, z - 1, -1];
        } else {
          params = [jdx, k, z + 1, 1];
        }
        return trace_from(...params);
      }
    }
    let success = trace_from(idx, i0, j0, dir);
    if (!success || out.length < 3) {
      return null;
    }
    return out;
  }

  let xmin = Infinity;
  let amin = null;
  for (let i = 0; i < poly0.length; i++) {
    if (poly0[i][0] < xmin) {
      xmin = poly0[i][0];
      amin = [0,i];
    }
  }
  for (let i = 0; i < poly1.length; i++) {
    if (poly1[i][0] < xmin) {
      xmin = poly1[i][0];
      amin = [1,i];
    }
  }

  function check_concavity(poly, idx) {
    let n = poly.length;
    let a = poly[(idx - 1 + n) % n];
    let b = poly[idx];
    let c = poly[(idx + 1) % n];
    let cw = pt_in_pl(...a, ...b, ...c) < 0 ? 1 : -1;
    return cw;
  }

  let cw = check_concavity(amin[0]?poly1:poly0, amin[1]);
  let ret = trace_outline(...amin, -1, cw, true);
  if (!ret) {
    return [];
  }
  return ret;
}

function seg_isect_poly(x0,y0,x1,y1,poly,is_ray=false){
  let n = poly.length;
  let isects = [];
  for (let i = 0; i < poly.length; i++){
    let a = poly[i];
    let b = poly[(i+1)%n];
    let xx = seg_isect(x0,y0,x1,y1,...a,...b,is_ray);
    if (xx){
      isects.push(xx);
    }
  }
  isects.sort((a,b)=>a.t-b.t);
  return isects;
}


function clip(polyline,polygon){
  if (!polyline.length){
    return {true:[],false:[]};
  }
  let zero = seg_isect_poly(...polyline[0],polyline[0][0]+Math.E,polyline[0][1]+PI,polygon,true).length % 2 != 0;
  let out = {
    'true' :[[]],
    'false':[[]],
  }
  let io = zero;
  for (let i = 0; i < polyline.length; i++){
    let a= polyline[i];
    let b= polyline[i+1];
    out[io][out[io].length-1].push(a);
    if (!b) break;

    let isects = seg_isect_poly(...a,...b,polygon,false);
    for (let j = 0; j < isects.length; j++){
      out[io][out[io].length-1].push(isects[j].xy);
      io = !io;
      out[io].push([isects[j].xy]);
    }
  }
  out.true = out.true.filter(x=>x.length);
  out.false = out.false.filter(x=>x.length);
  return out;
}

function clip_multi(polylines,polygon,clipper_func=clip){
  let out = {
    true:[],
    false:[],
  };
  for (let i = 0; i < polylines.length; i++){
    let c = clipper_func(polylines[i],polygon);
    out.true.push(...c.true);
    out.false.push(...c.false); 
  }
  return out;
}

function binclip(polyline,func){
  if (!polyline.length){
    return {true:[],false:[]};
  }
  let bins = [];
  for (let i = 0; i < polyline.length; i++){
    let t = i/(polyline.length-1);
    bins.push(func(...polyline[i],t));
  }
  let zero = bins[0];
  let out = {
    'true' :[[]],
    'false':[[]],
  }
  let io = zero;
  for (let i = 0; i < polyline.length; i++){
    let a= polyline[i];
    let b= polyline[i+1];
    out[io][out[io].length-1].push(a);
    if (!b) break;

    let do_isect = bins[i] != bins[i+1];

    if (do_isect){
      let pt = lerp2d(...a,...b,0.5);
      out[io][out[io].length-1].push(pt);
      io = !io;
      out[io].push([pt]);
    }
  }
  out.true = out.true.filter(x=>x.length);
  out.false = out.false.filter(x=>x.length);
  return out;
}


function shade_shape(poly,step=5,dx=10,dy=20){
  let bbox = get_bbox(poly);
  bbox.x -= step;
  bbox.y -= step;
  bbox.w += step*2;
  bbox.h += step*2;
  let lines = [];
  for (let i = -bbox.h; i < bbox.w; i+=step){
    let x0 = bbox.x + i;
    let y0 = bbox.y;
    let x1 = bbox.x + i + bbox.h;
    let y1 = bbox.y + bbox.h;
    lines.push([[x0,y0],[x1,y1]]);
  }
  lines = clip_multi(lines,poly).true;

  let carve = trsl_poly(poly,-dx,-dy);

  lines = clip_multi(lines,carve).false;

  for (let i = 0; i < lines.length; i++){
    let [a,b] = lines[i];
    let s = rand()*0.5;
    if (dy > 0){
      a = lerp2d(...a,...b,s);
      lines[i][0] = a;
    }else{
      b = lerp2d(...b,...a,s);
      lines[i][1] = b;
    }
  }

  return lines;
}


function fill_shape(poly,step=5){
  let bbox = get_bbox(poly);
  bbox.x -= step;
  bbox.y -= step;
  bbox.w += step*2;
  bbox.h += step*2;
  let lines = [];
  for (let i = 0; i < bbox.w+bbox.h/2; i+=step){
    let x0 = bbox.x + i;
    let y0 = bbox.y;
    let x1 = bbox.x + i - bbox.h/2;
    let y1 = bbox.y + bbox.h;
    lines.push([[x0,y0],[x1,y1]]);
  }
  lines = clip_multi(lines,poly).true;
  return lines;
}

function patternshade_shape(poly,step=5,pattern_func){
  let bbox = get_bbox(poly);
  bbox.x -= step;
  bbox.y -= step;
  bbox.w += step*2;
  bbox.h += step*2;
  let lines = [];
  for (let i = -bbox.h/2; i < bbox.w; i+=step){
    let x0 = bbox.x + i;
    let y0 = bbox.y;
    let x1 = bbox.x + i + bbox.h/2;
    let y1 = bbox.y + bbox.h;
    lines.push([[x0,y0],[x1,y1]]);
  }
  lines = clip_multi(lines,poly).true;

  for (let i = 0; i < lines.length; i++){
    lines[i] = resample(lines[i],2);
  }

  lines = clip_multi(lines,pattern_func,binclip).true;

  return lines;
}


function vein_shape(poly,n=50){
  let bbox = get_bbox(poly);
  let out = [];
  for (let i = 0; i < n; i++){
    let x = bbox.x + rand()*bbox.w;
    let y = bbox.y + rand()*bbox.h;
    let o = [[x,y]];
    for (let j = 0; j < 15; j++){
      let dx = (noise(x*0.1,y*0.1,7)-0.5)*4;
      let dy = (noise(x*0.1,y*0.1,6)-0.5)*4;
      x += dx;
      y += dy;
      o.push([x,y]);
    }
    out.push(o);
  }
  out = clip_multi(out,poly).true;
  return out;
}

function smalldot_shape(poly,scale=1){
  let samples = [];
  let bbox = get_bbox(poly);
  poissondisk(bbox.w,bbox.h,5*scale,samples);
  for (let i = 0; i < samples.length; i++){
    samples[i][0] += bbox.x;
    samples[i][1] += bbox.y;
  }
  let out = [];
  let n = 7;
  for (let i = 0; i < samples.length; i++){
    let [x,y] =samples[i]
    let t = (y > 0) ? (y/300) : 0.5;
    // console.log(y,t);
    if ((t > 0.4 || y < 0) && t > rand()){
      continue;
    }
    for (let k = 0; k < 2; k++){
      let o = [];
      for (let j = 0; j < n; j++){
        let t = j/(n-1);
        let a = t * PI * 2;
        o.push([
          Math.cos(a)*1-k*0.3,
          Math.sin(a)*0.5-k*0.3,
        ])
      }
      o = trsl_poly(rot_poly(o,rand()*PI*2),x,y);
      out.push(o);
    }
    
  }
  return clip_multi(out,poly).true;
}

function isect_circ_line(cx,cy,r,x0,y0,x1,y1){
  //https://stackoverflow.com/a/1084899
  let dx = x1-x0;
  let dy = y1-y0;
  let fx = x0-cx;
  let fy = y0-cy;
  let a = dx*dx+dy*dy;
  let b = 2*(fx*dx+fy*dy);
  let c = (fx*fx+fy*fy)-r*r;
  let discriminant = b*b-4*a*c;
  if (discriminant<0){
    return null;
  }
  discriminant = Math.sqrt(discriminant);
  let t0 = (-b - discriminant)/(2*a);
  if (0 <= t0 && t0 <= 1){
    return t0;
  }
  let t = (-b + discriminant)/(2*a);
  if (t > 1 || t < 0){
    return null;
  }
  return t;
}

function resample(polyline,step){
  if (polyline.length < 2){
    return polyline.slice();
  }
  polyline = polyline.slice();
  let out = [polyline[0].slice()];
  let next = null;
  let i = 0;
  while(i < polyline.length-1){
    let a = polyline[i];
    let b = polyline[i+1];
    let dx = b[0]-a[0];
    let dy = b[1]-a[1];
    let d = Math.sqrt(dx*dx+dy*dy);
    if (d == 0){
      i++;
      continue;
    }
    let n = ~~(d/step);
    let rest = (n*step)/d;
    let rpx = a[0] * (1-rest) + b[0] * rest;
    let rpy = a[1] * (1-rest) + b[1] * rest;
    for (let j = 1; j <= n; j++){
      let t = j/n;
      let x = a[0]*(1-t) + rpx*t;
      let y = a[1]*(1-t) + rpy*t;
      let xy = [x,y];
      for (let k = 2; k < a.length; k++){
        xy.push(a[k]*(1-t) + (a[k] * (1-rest) + b[k] * rest)*t);
      }
      out.push(xy);
    }

    next = null;
    for (let j = i+2; j < polyline.length; j++){
      let b = polyline[j-1];
      let c = polyline[j];
      if (b[0] == c[0] && b[1] == c[1]){
        continue;
      }
      let t = isect_circ_line(rpx,rpy,step,b[0],b[1],c[0],c[1]);
      if (t == null){
        continue;
      }
 
      let q = [
        b[0]*(1-t)+c[0]*t,
        b[1]*(1-t)+c[1]*t,
      ];
      for (let k = 2; k < b.length; k++){
        q.push(b[k]*(1-t)+c[k]*t);
      }
      out.push(q);
      polyline[j-1] = q;
      next = j-1;
      break;
    }
    if (next == null){
      break;
    }
    i = next;

  }

  if (out.length > 1){
    let lx = out[out.length-1][0];
    let ly = out[out.length-1][1];
    let mx = polyline[polyline.length-1][0];
    let my = polyline[polyline.length-1][1];
    let d = Math.sqrt((mx-lx)**2+(my-ly)**2);
    if (d < step*0.5){
      out.pop(); 
    }
  }
  out.push(polyline[polyline.length-1].slice());
  return out;
}


function pt_seg_dist(p, p0, p1)  {
  // https://stackoverflow.com/a/6853926
  let x = p[0];   let y = p[1];
  let x1 = p0[0]; let y1 = p0[1];
  let x2 = p1[0]; let y2 = p1[1];
  let A = x - x1; let B = y - y1; let C = x2 - x1; let D = y2 - y1;
  let dot = A*C+B*D;
  let len_sq = C*C+D*D;
  let param = -1;
  if (len_sq != 0) {
    param = dot / len_sq;
  }
  let xx; let yy;
  if (param < 0) {
    xx = x1; yy = y1;
  }else if (param > 1) {
    xx = x2; yy = y2;
  }else {
    xx = x1 + param*C;
    yy = y1 + param*D;
  }
  let dx = x - xx;
  let dy = y - yy;
  return Math.sqrt(dx*dx+dy*dy);
}

function approx_poly_dp(polyline, epsilon){
  if (polyline.length <= 2){
    return polyline;
  }
  let dmax   = 0;
  let argmax = -1;
  for (let i = 1; i < polyline.length-1; i++){
    let d = pt_seg_dist(polyline[i] , 
                        polyline[0] , 
                        polyline[polyline.length-1] );
    if (d > dmax){
      dmax = d;
      argmax = i;
    }  
  }
  let ret = [];
  if (dmax > epsilon){
    let L = approx_poly_dp(polyline.slice(0,argmax+1),epsilon);
    let R = approx_poly_dp(polyline.slice(argmax,polyline.length),epsilon);
    ret = ret.concat(L.slice(0,L.length-1)).concat(R);
  }else{
    ret.push(polyline[0].slice());
    ret.push(polyline[polyline.length-1].slice());
  }
  return ret;
}

function distsq(x0, y0, x1, y1) {
  let dx = x0-x1;
  let dy = y0-y1;
  return dx*dx+dy*dy;
}
function poissondisk(W, H, r, samples) {
  let grid = [];
  let active = [];
  let w =  ((r) / (1.4142135624));
  let r2 = ((r) * (r));
  let cols = (~~(((W) / (w))));
  let rows = (~~(((H) / (w))));
  for (let i = (0); Number((i) < (((cols) * (rows)))); i += (1)) {
    (grid).splice((grid.length), 0, (-1));
  };
  let pos = [(((W) / (2.0))), (((H) / (2.0)))];
  (samples).splice((samples.length), 0, (pos));
  for (let i = (0); Number((i) < (samples.length)); i += (1)) {
    let col = (~~(((((((samples)[i]))[0])) / (w))));
    let row = (~~(((((((samples)[i]))[1])) / (w))));
    ((grid)[((col) + (((row) * (cols))))] = i);
    (active).splice((active.length), 0, (((samples)[i])));
  };
  while (active.length) {
    let ridx = (~~(((rand()) * (active.length))));
    pos = ((active)[ridx]);
    let found = 0;
    for (let n = (0); Number((n) < (30)); n += (1)) {
      let sr = ((r) + (((rand()) * (r))));
      let sa = ((6.2831853072) * (rand()));
      let sx = ((((pos)[0])) + (((sr) * (Math.cos(sa)))));
      let sy = ((((pos)[1])) + (((sr) * (Math.sin(sa)))));
      let col = (~~(((sx) / (w))));
      let row = (~~(((sy) / (w))));
      if (((((((((Number((col) > (0))) && (Number((row) > (0))))) && (Number((col) < (((cols) - (1))))))) && (Number((row) < (((rows) - (1))))))) && (Number((((grid)[((col) + (((row) * (cols))))])) == (-1))))) {
        let ok = 1;
        for (let i = (-1); Number((i) <= (1)); i += (1)) {
          for (let j = (-1); Number((j) <= (1)); j += (1)) {
            let idx = ((((((((row) + (i))) * (cols))) + (col))) + (j));
            let nbr = ((grid)[idx]);
            if (Number((-1) != (nbr))) {
              let d = distsq(sx, sy, ((((samples)[nbr]))[0]), ((((samples)[nbr]))[1]));
              if (Number((d) < (r2))) {
                ok = 0;
              };
            };
          };
        };
        if (ok) {
          found = 1;
          ((grid)[((((row) * (cols))) + (col))] = samples.length);
          let sample = [(sx), (sy)];
          (active).splice((active.length), 0, (sample));
          (samples).splice((samples.length), 0, (sample));
        };
      };
    };
    if (Number(!(found))) {
      (active).splice((ridx), (1));
    };
  };
}


function draw_svg(polylines){
  let o = `<svg xmlns="http://www.w3.org/2000/svg" width="520" height="320">`
  o += `<rect x="0" y="0" width="520" height="320" fill="floralwhite"/><rect x="10" y="10" width="500" height="300" stroke="black" stroke-width="1" fill="none"/><path stroke="black" stroke-width="1" fill="none" stroke-linecap="round" stroke-linejoin="round" d="`
  for (let i = 0; i < polylines.length; i++){
    o += '\nM ';
    for (let j = 0; j < polylines[i].length; j++){
      let [x,y] = polylines[i][j];
      o += `${(~~((x+10)*100)) /100} ${(~~((y+10)*100)) /100} `;
    }
  }
  o += `\n"/></svg>`
  return o;
}

function draw_svg_anim(polylines,speed){
  let o = `<svg xmlns="http://www.w3.org/2000/svg" width="520" height="320">`;
  o += `<rect x="0" y="0" width="520" height="320" fill="floralwhite"/><rect x="10" y="10" width="500" height="300" stroke="black" stroke-width="1" fill="none"/>`
  let lengths = [];
  let acc_lengths = [];
  let total_l = 0;
  for (let i = 0; i < polylines.length; i++){
    let l = 0;
    for (let j = 1; j < polylines[i].length; j++){
      l += Math.hypot(
        polylines[i][j-1][0]-polylines[i][j][0],
        polylines[i][j-1][1]-polylines[i][j][1]
      );
    }
    lengths.push(l);
    acc_lengths.push(total_l);
    total_l+=l;
  }
  for (let i = 0; i < polylines.length; i++){
    let l = lengths[i];
    o += `
    <path 
      stroke="black" 
      stroke-width="1" 
      fill="none" 
      stroke-dasharray="${l}"
      stroke-dashoffset="${l}"
      d="M`;
    for (let j = 0; j < polylines[i].length; j++){
      o += polylines[i][j] + ' ';
    }
    let t = speed*l;
    o += `">
    <animate id="a${i}"
      attributeName="stroke-dashoffset" 
      fill="freeze"
      from="${l}" to="${0}" dur="${t}s" 
      begin="${(acc_lengths[i])*speed}s;a${i}.end+${8+speed*total_l-t}s"/>
    />
    <animate id="b${i}"
      attributeName="stroke-dashoffset" 
      fill="freeze"
      from="${0}" to="${l}" dur="${3}s" 
      begin="${5+speed*total_l}s;b${i}.end+${5+speed*total_l}s"/>
    />
    </path>`;
  }
  o += `</svg>`;
  return o;
}

function draw_ps(polylines){
  let o = `%!PS-Adobe-3.0 EPSF-3.0
%%BoundingBox: 0 0 520 320
1 setlinewidth
0.5 0.5 translate
/m /moveto load def
/l /lineto load def
/F /stroke load def
%%EndPageSetup
10 10 m
510 10 l
510 310 l
10 310 l
closepath
F
`;
  for (let i = 0; i < polylines.length; i++){
    for (let j = 0; j < polylines[i].length; j++){
      let [x,y] = polylines[i][j];
      o += `${(~~((x+10)*100)) /100} ${(~~((310-y)*100)) /100} `;
      if (j == 0) {
        o += `m\n`;
      } else {
        o += `l\n`;
      }
    }
    o += `F\n\n`;
  }
  return o;
}




function pow(a,b){
  return Math.sign(a) * Math.pow(Math.abs(a),b);
}

function gauss2d(x, y){
  let z0 = Math.exp(-0.5*x*x);
  let z1 = Math.exp(-0.5*y*y);
  return z0*z1;
 }

function squama_mask(w,h){
  let p = [];
  let n = 7;
  for (let i = 0; i < n; i++){
    let t = i/n;
    let a = t * PI * 2;
    let x = -pow(Math.cos(a),1.3)*w;
    let y =  pow(Math.sin(a),1.3)*h;
    p.push([x,y]);
  }
  return p;
}

function squama(w,h,m=3) {
  let p = [];
  let n = 8;
  for (let i = 0; i < n; i++){
    let t = i/(n-1);
    let a = t * PI + PI/2;
    let x = -pow(Math.cos(a),1.4)*w;
    let y =  pow(Math.sin(a),1.4)*h;
    p.push([x,y]);
  }
  let q = [p];
  for (let i = 0; i < m; i++){
    let t = i/(m-1);
    q.push([
      [-w*0.3 + (rand()-0.5),-h*0.2+t*h*0.4 + (rand()-0.5)],
      [ w*0.5 + (rand()-0.5),-h*0.3+t*h*0.6 + (rand()-0.5)]
    ]);
  }
  return q;
}

function trsl_poly(poly,x,y){
  return poly.map(xy=>[xy[0]+x,xy[1]+y]);
}
function scl_poly(poly,sx,sy){
  if (sy === undefined) sy = sx;
  return poly.map(xy=>[xy[0]*sx,xy[1]*sy]);
}
function shr_poly(poly,sx){
  return poly.map(xy=>[xy[0]+xy[1]*sx,xy[1]]);
}
function rot_poly(poly,th){
  let qoly = [];
  let costh = Math.cos(th);
  let sinth = Math.sin(th);
  for (let i = 0; i < poly.length; i++){
    let [x0,y0] = poly[i]
    let x = x0* costh-y0*sinth;
    let y = x0* sinth+y0*costh;
    qoly.push([x,y]);
  }
  return qoly;
}

function squama_mesh(m,n,uw,uh,squama_func,noise_x,noise_y,interclip=true){
  let clipper = null;

  let pts = [];
  for (let i = 0; i < n; i++){
    for (let j = 0; j < m; j++){
      let x = j*uw;
      let y = (n*uh/2) - Math.cos(i/(n-1) * PI) * (n*uh/2);
      let a = noise(x*0.005,y*0.005)*PI*2-PI;
      let r = noise(x*0.005,y*0.005);
      let dx = Math.cos(a)*r*noise_x;
      let dy = Math.cos(a)*r*noise_y;
      pts.push([x+dx,y+dy]);
    }
  }
  let out = [];

  let whs = [];
  for (let i = 0; i < n; i++){
    for (let j = 0; j < m; j++){
      if (i == 0 || j == 0 || i == n-1 || j == m-1){
        whs.push([uw/2,uh/2]);
        continue;
      }
      let a = pts[i*m+j];
      let b = pts[i*m+j+1];
      let c = pts[i*m+j-1];
      let d = pts[(i-1)*m+j];
      let e = pts[(i+1)*m+j];

      let dw = (dist(...a,...b) + dist(...a,...c))/4
      let dh = (dist(...a,...d) + dist(...a,...e))/4
      whs.push([dw,dh]);
    }
  }

  for (let j = 1; j < m-1; j++){
    for (let i = 1; i < n-1; i++){
      let [x,y]  = pts[i*m+j];
      let [dw,dh]= whs[i*m+j];
      let q = trsl_poly(squama_mask(dw,dh),x,y);

      let p = squama_func(x,y,dw,dh).map(a=>trsl_poly(a,x,y));
      if (!interclip){
        out.push(...p);
      }else{
        if (clipper){
          out.push(...clip_multi(p,clipper).false);
          clipper = poly_union(clipper,q);
        }else{
          out.push(...p);
          clipper = q;
        }
      }
    }
    for (let i = 1; i < n-1; i++){
      let a = pts[i*m+j];
      let b = pts[i*m+j+1];
      let c = pts[(i+1)*m+j];
      let d = pts[(i+1)*m+j+1];

      let [dwa,dha] = whs[i*m+j];
      let [dwb,dhb] = whs[i*m+j+1];
      let [dwc,dhc] = whs[(i+1)*m+j];
      let [dwd,dhd] = whs[(i+1)*m+j+1];

      let [x,y] = [(a[0]+b[0]+c[0]+d[0])/4,(a[1]+b[1]+c[1]+d[1])/4];
      let [dw,dh] = [(dwa+dwb+dwc+dwd)/4,(dha+dhb+dhc+dhd)/4];
      dw *= 1.2;
      let q = trsl_poly(squama_mask(dw,dh),x,y);

      let p = squama_func(x,y,dw,dh).map(a=>trsl_poly(a,x,y));
      if (!interclip){
        out.push(...p);
      }else{
        if (clipper){
          out.push(...clip_multi(p,clipper).false);
          clipper = poly_union(clipper,q);
        }else{
          out.push(...p);
          clipper = q;
        }
      }
    }
  }
  // for (let i = 0; i < n-1; i++){
  //   for (let j = 0; j < m-1; j++){
  //     let a= pts[i*m+j];
  //     let b= pts[i*m+j+1];
  //     let c = pts[(i+1)*m+j];
  //     out.push([a,b]);
  //     out.push([a,c]);
  //   }
  // }
  return out;
}

function pattern_dot(scale=1){
  let samples = [];
  poissondisk(500,300,20*scale,samples);
  let rs = [];
  for (let i = 0; i < samples.length; i++){
    rs.push((rand()*5+10)*scale)
  }
  return function(x,y){
    for (let i = 0; i < samples.length; i++){
      let r = rs[i];
      if (dist(x,y,...samples[i])<r){

        let [x0,y0] = samples[i];
        let dx = x - x0;
        let dy = y - y0;
        if (gauss2d(dx/r*2,dy/r*2)*noise(x,y,999) > 0.2){
          return true;
        }
      }
    }
    return false;
  }
}




function fish_body_a(curve0,curve1,scale_scale,pattern_func){
  let curve2 = [];
  let curve3 = [];
  for (let i = 0; i < curve0.length; i++){
    curve2.push(lerp2d(...curve0[i],...curve1[i],0.95));
    curve3.push(lerp2d(...curve0[i],...curve1[i],0.85));
  }
  let outline1 = curve0.concat(curve1.slice().reverse());
  let outline2 = curve0.concat(curve2.slice().reverse());
  let outline3 = curve0.concat(curve3.slice().reverse());

  let bbox = get_bbox(curve0.concat(curve1));
  let m = ~~(bbox.w/(scale_scale*15));
  let n = ~~(bbox.h/(scale_scale*15));
  let uw = bbox.w/m;
  let uh = bbox.h/n;

  let fn = pattern_func?((x,y,w,h)=>squama(w,h,Number(pattern_func(x,y))*3)):((x,y,w,h)=>squama(w,h))
  let sq = squama_mesh(m,n+3,uw,uh,fn,uw*3,uh*3,true).map(a=>trsl_poly(a,bbox.x,bbox.y-uh*1.5));
  let o0 = clip_multi(sq,outline2)[true];
  let o1 = clip_multi(o0,outline3);
  o1.false = o1.false.filter(x=>rand()<0.6);
  let o = [];
  o.push(curve0,curve1.slice().reverse(),...o1.true,...o1.false);
  return o;
}

function fish_body_b(curve0,curve1,scale_scale,pattern_func){
  let curve2 = [];
  for (let i = 0; i < curve0.length; i++){
    curve2.push(lerp2d(...curve0[i],...curve1[i],0.95));
  }
  let outline1 = curve0.concat(curve1.slice().reverse());
  let outline2 = curve0.concat(curve2.slice().reverse());

  let bbox = get_bbox(curve0.concat(curve1));
  let m = ~~(bbox.w/(scale_scale*5));
  let n = ~~(bbox.h/(scale_scale*5));
  let uw = bbox.w/m;
  let uh = bbox.h/n;

  let sq = squama_mesh(m,n+16,uw,uh,(x,y,w,h)=>squama(w*0.7,h*0.6,0),uw*8,uh*8,false).map(a=>trsl_poly(a,bbox.x,bbox.y-uh*8));
  let o0 = clip_multi(sq,outline2)[true];

  let o1 = [];
  for (let i = 0; i < o0.length; i++){
    let [x,y] = o0[i][0];
    let t = (y-bbox.y)/bbox.h;
    // if (rand() > t){
    //   o1.push(o0[i]);
    // }
    // if ((~~(x/30))%2 || (rand() > t && rand()>t)){
    //   o1.push(o0[i]);
    // }
    if (pattern_func){
      if (pattern_func(x,y) || (rand() > t && rand()>t)) {
        o1.push(o0[i]);
      }
    }else{
      if (rand() > t){
        o1.push(o0[i]);
      }
    }
  }
  let o = [];
  o.push(curve0,curve1.slice().reverse(),...o1);
  return o;
}


function ogee(x){
  return 4 * Math.pow(x-0.5,3) + 0.5;
}

function fish_body_c(curve0,curve1,scale_scale){
  let step = 6*scale_scale;

  let curve2 = [];
  let curve3 = [];

  for (let i = 0; i < curve0.length; i++){
    curve2.push(lerp2d(...curve0[i],...curve1[i],0.95));
    curve3.push(lerp2d(...curve0[i],...curve1[i],0.4));
  }
  let outline1 = curve0.concat(curve1.slice().reverse());
  let outline2 = curve0.concat(curve2.slice().reverse());

  let bbox = get_bbox(curve0.concat(curve1));
  bbox.x -= step;
  bbox.y -= step;
  bbox.w += step*2;
  bbox.h += step*2;

  let lines = [curve3.reverse()];

  for (let i = -bbox.h; i < bbox.w; i+=step){
    let x0 = bbox.x + i;
    let y0 = bbox.y;
    let x1 = bbox.x + i + bbox.h;
    let y1 = bbox.y + bbox.h;
    lines.push([[x0,y0],[x1,y1]]);
  }
  for (let i = 0; i < bbox.w+bbox.h; i+=step){
    let x0 = bbox.x + i;
    let y0 = bbox.y;
    let x1 = bbox.x + i - bbox.h;
    let y1 = bbox.y + bbox.h;
    lines.push([[x0,y0],[x1,y1]]);
  }
  for (let i = 0; i < lines.length; i++){
    lines[i] = resample(lines[i],4);
    for (let j = 0;j < lines[i].length; j++){
      let [x,y] = lines[i][j];
      let t = (y-bbox.y)/bbox.h;
      let y1 = -Math.cos(t*PI)*bbox.h/2+bbox.y+bbox.h/2;

      let dx = (noise(x*0.005,y1*0.005,0.1)-0.5)*50;
      let dy = (noise(x*0.005,y1*0.005,1.2)-0.5)*50;

      lines[i][j][0] += dx;
      lines[i][j][1] = y1 + dy;
    }
  }

  let o0 = clip_multi(lines,outline2)[true];

  o0 = clip_multi(o0,(x,y,t)=>(rand()>t||rand()>t),binclip).true;
  

  let o = [];
  
  o.push(curve0,curve1.slice().reverse(),...o0);
  return o;
}

function fish_body_d(curve0,curve1,scale_scale){
  let curve2 = [];
  for (let i = 0; i < curve0.length; i++){
    curve2.push(lerp2d(...curve0[i],...curve1[i],0.4));
  }
  curve0 = resample(curve0,10*scale_scale);
  curve1 = resample(curve1,10*scale_scale);
  curve2 = resample(curve2,10*scale_scale);

  let outline1 = curve0.concat(curve1.slice().reverse());
  let outline2 = curve0.concat(curve2.slice().reverse());

  let o0 = [curve2];
  for (let i = 3; i < Math.min(curve0.length,curve1.length,curve2.length); i++){
    let a = [curve0[i],curve2[i-3]];
    let b = [curve2[i-3],curve1[i]];
    
    o0.push(a,b);
  }

  let o1 = [];
  for (let i = 0; i < o0.length; i++){
    o0[i] = resample(o0[i],4);
    for (let j = 0; j < o0[i].length; j++){
      let [x,y] = o0[i][j];
      let dx = 30*(noise(x*0.01,y*0.01,-1)-0.5);
      let dy = 30*(noise(x*0.01,y*0.01,9)-0.5);
      o0[i][j][0] += dx;
      o0[i][j][1] += dy;
    }
    
    o1.push(...binclip(o0[i],(x,y,t)=>(
      (rand()>Math.cos(t*PI) && rand() < x/500) || (rand()>Math.cos(t*PI) && rand() < x/500)
    )).true);
  }
  o1 = clip_multi(o1,outline1).true;

  let sh = vein_shape(outline1);

  let o = [];
  o.push(curve0,curve1.slice().reverse(),...o1,...sh);
  return o;
}




function fin_a(curve,ang0,ang1,func,clip_root=false,curvature0=0,curvature1=0,softness=10){
  let angs = [];
  for (let i = 0; i < curve.length; i++){
    
    if (i == 0){
      angs.push( Math.atan2(curve[i+1][1]-curve[i][1], curve[i+1][0]-curve[i][0]) - PI/2 );
    }else if (i == curve.length-1){
      angs.push( Math.atan2(curve[i][1]-curve[i-1][1], curve[i][0]-curve[i-1][0]) - PI/2 );
    }else{
      let a0 = Math.atan2(curve[i-1][1]-curve[i][1], curve[i-1][0]-curve[i][0]);
      let a1 = Math.atan2(curve[i+1][1]-curve[i][1], curve[i+1][0]-curve[i][0]);
      while (a1 > a0){
        a1 -= PI*2;
      }
      a1 += PI*2;
      let a = (a0+a1)/2;
      angs.push(a);
    }
  }
  let out0 = [];
  let out1 = [];
  let out2 = [];
  let out3 = [];
  for (let i = 0; i < curve.length; i++){
    let t = i/(curve.length-1);
    let aa = lerp(ang0,ang1,t);
    let a = angs[i]+aa;
    let w = func(t);

    let [x0,y0] = curve[i];
    let x1 = x0 + Math.cos(a)*w;
    let y1 = y0 + Math.sin(a)*w;
    
    let p = resample([[x0,y0],[x1,y1]],3);
    for (let j = 0; j < p.length; j++){
      let s = j/(p.length-1);
      let ss = Math.sqrt(s);
      let [x,y] = p[j];
      let cv = lerp(curvature0,curvature1,t)*Math.sin(s*PI);
      p[j][0] += noise(x*0.1,y*0.1,3)*ss*softness + Math.cos(a-PI/2)*cv;
      p[j][1] += noise(x*0.1,y*0.1,4)*ss*softness + Math.sin(a-PI/2)*cv;
    }
    if (i == 0){
      out2 = p;
    }else if (i == curve.length-1){
      out3 = p.slice().reverse();
    }else{
      out0.push(p[p.length-1]);
      // if (i % 2){
        let q = p.slice(clip_root?(   ~~(rand()*4)  ):0,Math.max(2,~~(p.length*(rand()*0.5+0.5))));
        if (q.length){
          out1.push(q);
        }
      // }
    }
  }
  out0 = resample(out0,3);
  for (let i = 0; i < out0.length; i++){
    let [x,y] = out0[i];
    out0[i][0] += (noise(x*0.1,y*0.1)*6-3)*(softness/10);
    out0[i][1] += (noise(x*0.1,y*0.1)*6-3)*(softness/10);
  }
  let o = out2.concat(out0).concat(out3);
  out1.unshift(o);
  return [o.concat(curve.slice().reverse()),out1];
}


function fin_b(curve,ang0,ang1,func,dark=1){
  let angs = [];
  for (let i = 0; i < curve.length; i++){
    
    if (i == 0){
      angs.push( Math.atan2(curve[i+1][1]-curve[i][1], curve[i+1][0]-curve[i][0]) - PI/2 );
    }else if (i == curve.length-1){
      angs.push( Math.atan2(curve[i][1]-curve[i-1][1], curve[i][0]-curve[i-1][0]) - PI/2 );
    }else{
      let a0 = Math.atan2(curve[i-1][1]-curve[i][1], curve[i-1][0]-curve[i][0]);
      let a1 = Math.atan2(curve[i+1][1]-curve[i][1], curve[i+1][0]-curve[i][0]);
      while (a1 > a0){
        a1 -= PI*2;
      }
      a1 += PI*2;
      let a = (a0+a1)/2;
      angs.push(a);
    }
  }
  
  let out0 = [];
  let out1 = [];
  let out2 = [];
  let out3 = [];
  for (let i = 0; i < curve.length; i++){
    let t = i/(curve.length-1);
    let aa = lerp(ang0,ang1,t);
    let a = angs[i]+aa;
    let w = func(t);

    let [x0,y0] = curve[i];
    let x1 = x0 + Math.cos(a)*w;
    let y1 = y0 + Math.sin(a)*w;

    let b = [
      x1 + 0.5 * Math.cos(a-PI/2),
      y1 + 0.5 * Math.sin(a-PI/2),
    ];
    let c = [
      x1 + 0.5 * Math.cos(a+PI/2),
      y1 + 0.5 * Math.sin(a+PI/2),
    ];

    let p = [
      curve[i][0] + 1.8 * Math.cos(a-PI/2),
      curve[i][1] + 1.8 * Math.sin(a-PI/2),
    ];
    let q = [
      curve[i][0] + 1.8 * Math.cos(a+PI/2),
      curve[i][1] + 1.8 * Math.sin(a+PI/2),
    ];
    out1.push([x1,y1]);
    out0.push([p,b,c,q]);
  }

  let n = 10;
  for (let i = 0; i < curve.length-1; i++){

    let [_,__,a0,q0] = out0[i];
    let [p1,a1,___,____] = out0[i+1];

    let b = lerp2d(...a0,...q0,0.1);
    let c = lerp2d(...a1,...p1,0.1);

    let o = [];
    let ang = Math.atan2(c[1]-b[1],c[0]-b[0]);

    for (let j = 0; j < n; j++){
      let t = j/(n-1);
      let d = Math.sin(t*PI)*2;
      let a = lerp2d(...b,...c,t);
      o.push([
        a[0] + Math.cos(ang+PI/2)*d,
        a[1] + Math.sin(ang+PI/2)*d,
      ])
    }
    
    // out2.push([b,c]);
    out2.push(o);

    let m = ~~( Math.min(dist(...a0,...q0),dist(...a1,...p1) ) /10 * dark);
    let e = lerp2d(...curve[i],...curve[i+1],0.5);
    for (let k = 0; k < m; k ++){
      let p = [];
      let s= k/m*0.7;
      for (let j = 1; j < n-1; j++){
        p.push(lerp2d(...o[j],...e,s));
      }
      out3.push(p);
    }
  }

  let out4 = [];
  if (out0.length > 1){
    let clipper = out0[0];
    out4.push(out0[0])
    for (let i = 1; i < out0.length; i++){
      out4.push(...clip(out0[i],clipper).false);
      clipper = poly_union(clipper,out0[i]);
    }
  }

  return [out2.flat().concat(curve.slice().reverse()),out4.concat(out2).concat(out3)];
}


function finlet(curve,h,dir=1){
  let angs = [];
  for (let i = 0; i < curve.length; i++){
    if (i == 0){
      angs.push( Math.atan2(curve[i+1][1]-curve[i][1], curve[i+1][0]-curve[i][0]) - PI/2 );
    }else if (i == curve.length-1){
      angs.push( Math.atan2(curve[i][1]-curve[i-1][1], curve[i][0]-curve[i-1][0]) - PI/2 );
    }else{
      let a0 = Math.atan2(curve[i-1][1]-curve[i][1], curve[i-1][0]-curve[i][0]);
      let a1 = Math.atan2(curve[i+1][1]-curve[i][1], curve[i+1][0]-curve[i][0]);
      while (a1 > a0){
        a1 -= PI*2;
      }
      a1 += PI*2;
      let a = (a0+a1)/2;
      angs.push(a);
    }
  }
  let out0 = [];
  for (let i = 0; i < curve.length; i++){
    let t = i/(curve.length-1);
    let a = angs[i];
    let w = (i+1) % 3 ? 0 : h;
    if (dir > 0){
      w *= (1-t*0.5);
    }else{
      w *= 0.5 + t * 0.5
    }

    let [x0,y0] = curve[i];
    let x1 = x0 + Math.cos(a)*w;
    let y1 = y0 + Math.sin(a)*w;
    out0.push([x1,y1]);

  }
  out0 = resample(out0,2);
  for (let i = 0; i < out0.length; i++){
    let [x,y] = out0[i];
    out0[i][0] += noise(x*0.1,y*0.1)*2-3;
    out0[i][1] += noise(x*0.1,y*0.1)*2-3;
  }
  out0.push(curve[curve.length-1]);
  return [out0.concat(curve.slice().reverse()),[out0]];
}

function fin_adipose(curve,dx,dy,r){
  let n = 20;
  let [x0,y0] = curve[~~(curve.length/2)];
  let [x,y] = [x0+dx,y0+dy];
  let [x1,y1] = curve[0];
  let [x2,y2] = curve[curve.length-1];
  let d1 = dist(x,y,x1,y1);
  let d2 = dist(x,y,x2,y2);
  let a1 = Math.acos(r/d1);
  let a2 = Math.acos(r/d2);
  let a01 = Math.atan2(y1-y,x1-x)+a1;
  let a02 = Math.atan2(y2-y,x2-x)-a2;
  a02 -= PI*2;
  while (a02 < a01){
    a02 += PI*2;
  }
  let out0 = [[x1,y1]]
  for (let i = 0; i < n; i++){
    let t = i/(n-1);
    let a = lerp(a01,a02,t);
    let p = [
      x+Math.cos(a)*r,
      y+Math.sin(a)*r,
    ]
    out0.push(p);
  }
  out0.push([x2,y2]);
  out0 = resample(out0,3);
  for (let i = 0; i < out0.length; i++){
    let t = i/(out0.length-1);
    let s = Math.sin(t*PI);
    let [x,y] = out0[i];
    out0[i][0] += (noise(x*0.01,y*0.01)-0.5)*s*50;
    out0[i][1] += (noise(x*0.01,y*0.01)-0.5)*s*50;
  }
  let cc = out0.concat(curve.slice().reverse());
  let out1 = clip(trsl_poly(out0,0,4),cc).true;

  out1 = clip_multi(out1,(x,y,t)=>(rand()<Math.sin(t*PI)),binclip).true;
  return [cc,[out0,...out1]];

}


function fish_lip(x0,y0,x1,y1,w){
  x0 += rand()*0.001-0.0005;
  y0 += rand()*0.001-0.0005;
  x1 += rand()*0.001-0.0005;
  y1 += rand()*0.001-0.0005;
  let h = dist(x0,y0,x1,y1);
  let a0 = Math.atan2(y1-y0,x1-x0);
  let n = 10;
  let ang = Math.acos(w/h);
  let dx = Math.cos(a0+PI/2)*0.5;
  let dy = Math.sin(a0+PI/2)*0.5;
  let o = [[x0-dx,y0-dy]];
  for (let i = 0; i < n; i++){
    let t = i/(n-1);
    let a = lerp(ang,PI*2-ang,t) + a0;
    let x = -Math.cos(a)*w + x1;
    let y = -Math.sin(a)*w + y1;
    o.push([x,y]);
  }
  o.push([x0+dx,y0+dy]);
  o = resample(o,2.5);
  for (let i = 0; i < o.length; i++){
    let [x,y] = o[i];
    o[i][0] += noise(x*0.05,y*0.05,-1)*2-1;
    o[i][1] += noise(x*0.05,y*0.05,-2)*2-1;
  }
  return o;
}

function fish_teeth(x0,y0,x1,y1,h,dir,sep=3.5){
  let n = Math.max(2,~~(dist(x0,y0,x1,y1)/sep));
  let ang = Math.atan2(y1-y0,x1-x0);
  let out = [];
  for (let i = 0; i < n; i++){
    let t = i/(n-1);
    let a = lerp2d(x0,y0,x1,y1,t);
    let w = h*t;
    let b = [
      a[0]+Math.cos(ang+dir*PI/2)*w,
      a[1]+Math.sin(ang+dir*PI/2)*w,
    ];
    let c = [
      a[0] + 1 * Math.cos(ang),
      a[1] + 1 * Math.sin(ang),
    ];
    let d = [
      a[0] + 1 * Math.cos(ang+PI),
      a[1] + 1 * Math.sin(ang+PI),
    ];
    let e = lerp2d(...c,...b,0.7);
    let f = lerp2d(...d,...b,0.7);
    let g = [
      a[0]+Math.cos(ang+dir*(PI/2+0.15))*w,
      a[1]+Math.sin(ang+dir*(PI/2+0.15))*w,
    ]
    out.push([c,e,g,f,d])
    // out.push(barbel(...a,10,ang+dir*PI/2))
  }
  return out;
}

function fish_jaw(x0,y0,x1,y1,x2,y2){
  let n = 10;
  let ang = Math.atan2(y2-y0,x2-x0);
  let d = dist(x0,y0,x2,y2);
  let o = [];
  for (let i = 0; i < n; i++){
    let t = i/(n-1);
    let s = Math.sin(t*PI);
    let w = s*d/20;
    let p = lerp2d(x2,y2,x0,y0,t);
    let q = [
      p[0] + Math.cos(ang-PI/2)*w,
      p[1] + Math.sin(ang-PI/2)*w,
    ]
    let qq = [
      q[0] + (noise(q[0]*0.01,q[1]*0.01,1)-0.5)*4*s,
      q[1] + (noise(q[0]*0.01,q[1]*0.01,4)-0.5)*4*s,
    ];
    o.push(qq);
  }
  return [[[x2,y2],[x1,y1],[x0,y0]],[o,...vein_shape(o,5)]];
}


function fish_eye_a(ex,ey,rad){
  let n = 20;
  let eye0 = [];
  let eye1 = [];
  let eye2 = [];
  for (let i = 0; i < n; i++){
    let t = i/(n-1);
    let a = t * PI*2 + Math.PI/4*3;
    eye0.push([
      ex + Math.cos(a)*rad,
      ey + Math.sin(a)*rad
    ]);
    if (t > 0.5){
      eye1.push([
        ex + Math.cos(a)*(rad*0.8),
        ey + Math.sin(a)*(rad*0.8)
      ]);
    }
    eye2.push([
      ex + Math.cos(a)*(rad*0.4) -0.75,
      ey + Math.sin(a)*(rad*0.4) -0.75
    ]);
  }

  let ef = shade_shape(eye2,2.7,10,10);
  return [eye0,[eye0,eye1,eye2,...ef]];
}

function fish_eye_b(ex,ey,rad){
  let n = 20;
  let eye0 = [];
  let eye1 = [];
  let eye2 = [];
  for (let i = 0; i < n; i++){
    let t = i/(n-1);
    let a = t * PI*2+Math.E;
    eye0.push([
      ex + Math.cos(a)*rad,
      ey + Math.sin(a)*rad
    ]);
    eye2.push([
      ex + Math.cos(a)*(rad*0.4),
      ey + Math.sin(a)*(rad*0.4)
    ]);
  }
  let m =~~((rad*0.6)/2);
  for (let i = 0; i < m; i++){
    let r = rad - i * 2;
    let e = [];
    for (let i = 0; i < n; i++){
      let t = i/(n-1);
      let a = lerp(PI*7/8,PI*13/8,t)
      e.push([
        ex + Math.cos(a)*r,
        ey + Math.sin(a)*r
      ]);
    
    }
    eye1.push(e);
  }
  let trig = [
    [ex+Math.cos(-PI*3/4)*(rad*0.9),ey+Math.sin(-PI*3/4)*(rad*0.9)],
    [ex+1,ey+1],
    [ex+Math.cos(-PI*11/12)*(rad*0.9),ey+Math.sin(-PI*11/12)*(rad*0.9)],
  ];
  trig = resample(trig,3);
  for (let i = 0; i < trig.length; i++){
    let [x,y] = trig[i];
    x += noise(x*0.1,y*0.1,22)*4-2;
    y += noise(x*0.1,y*0.1,33)*4-2;
    trig[i] = [x,y];
  }
  

  let ef = fill_shape(eye2,1.5);

  ef = clip_multi(ef,trig).false;
  eye1 = clip_multi(eye1,trig).false;
  eye2 = clip(eye2,trig).false;

  return [eye0,[eye0,...eye1,...eye2,...ef]];
}


function barbel(x,y,n,ang,dd=3){
  let curve = [[x,y]];
  let sd = rand()*PI*2;
  let ar = 1;
  for (let i = 0; i < n; i++){
    x += Math.cos(ang)*dd;
    y += Math.sin(ang)*dd;
    ang += (noise(i*0.1,sd)-0.5)*ar;
    if (i < n/2){
      ar *= 1.02;
    }else{
      ar *= 0.92;
    }
    curve.push([x,y]);
  }
  let o0 = [];
  let o1 = [];
  for (let i = 0; i < n-1; i++){
    let t = i/(n-1);
    let w = 1.5*(1-t);

    let a = curve[i-1];
    let b = curve[i];
    let c = curve[i+1];

    let a1 = Math.atan2(c[1]-b[1],c[0]-b[0]);
    let a2;

    if (a){
      let a0 = Math.atan2(a[1]-b[1],a[0]-b[0]);
      
      a1 -= PI*2;
      while (a1 < a0){
        a1 += PI*2;
      }
      a2 = (a0+a1)/2;
    }else{
      a2 = a1-PI/2;
    }

    o0.push([
      b[0]+Math.cos(a2)*w,
      b[1]+Math.sin(a2)*w
    ])
    o1.push([
      b[0]+Math.cos(a2+PI)*w,
      b[1]+Math.sin(a2+PI)*w
    ])
  }
  o0.push(curve[curve.length-1]);
  return o0.concat(o1.slice().reverse());
}

function fish_head(x0,y0,x1,y1,x2,y2,arg){
  let n = 20;
  let curve0 = [];
  let curve1 = [];
  let curve2 = [];
  for (let i = 0; i < n; i++){
    let t = i/(n-1);
    let a = PI/2 * t;
    let x = x1-pow(Math.cos(a),1.5)*(x1-x0);
    let y = y0-pow(Math.sin(a),1.5)*(y0-y1);
    // let x = lerp(x0,x1,t);
    // let y = lerp(y0,y1,t);

    let dx = (noise(x*0.01,y*0.01,9)*40-20)*(1.01-t);
    let dy = (noise(x*0.01,y*0.01,8)*40-20)*(1.01-t);
    curve0.push([x+dx,y+dy]);
  }
  for (let i = 0; i < n; i++){
    let t = i/(n-1);
    let a = PI/2 * t;
    let x = x2-pow(Math.cos(a),0.8)*(x2-x0);
    let y = y0+pow(Math.sin(a),1.5)*(y2-y0);

    let dx = (noise(x*0.01,y*0.01,9)*40-20)*(1.01-t);
    let dy = (noise(x*0.01,y*0.01,8)*40-20)*(1.01-t);
    curve1.unshift([x+dx,y+dy]);
  }
  let ang = Math.atan2(y2-y1,x2-x1);
  for (let i = 1; i < n-1; i++){
    let t = i/(n-1);
    let p = lerp2d(x1,y1,x2,y2,t);
    let s = pow(Math.sin(t*Math.PI),0.5);
    let r = noise(t*2,1.2) * s * 20;

    let dx = Math.cos(ang-Math.PI/2) * r;
    let dy = Math.sin(ang-Math.PI/2) * r;
    curve2.push([ p[0]+dx,p[1]+dy ])
  }
  let outline = curve0.concat(curve2).concat(curve1);

  let inline = curve2.slice(~~(curve2.length/3)).concat(curve1.slice(0,~~(curve1.length/2))).slice(0,curve0.length);
  for (let i = 0; i < inline.length; i++){
    let t = i/(inline.length-1);
    let s = Math.sin(t*PI)**2*0.1+0.12;
    inline[i] = lerp2d(...inline[i],...curve0[i],s);
  }
  let dix = (x0-inline[inline.length-1][0])*0.3;
  let diy = (y0-inline[inline.length-1][1])*0.2;
  for (let i = 0; i < inline.length; i++){
    inline[i][0] += dix;
    inline[i][1] += diy;
  }


  let par = [0.475,0.375];
  let ex = x0*par[0] + x1*par[1] + x2*(1-par[0]-par[1]);
  let ey = y0*par[0] + y1*par[1] + y2*(1-par[0]-par[1]);
  let d0 = pt_seg_dist([ex,ey],[x0,y0],[x1,y1]);
  let d1 = pt_seg_dist([ex,ey],[x0,y0],[x2,y2]);
  if (d0 < arg.eye_size && d1 < arg.eye_size){
    arg.eye_size = Math.min(d0,d1);
  }else if (d0 < arg.eye_size){
    let ang = Math.atan2(y1-y0,x1-x0)+PI/2;
    ex = x0*0.5+x1*0.5 + Math.cos(ang)*arg.eye_size;
    ey = y0*0.5+y1*0.5 + Math.sin(ang)*arg.eye_size;
  }

  let jaw_pt0 = curve1[18-arg.mouth_size];
  let jaw_l = dist(...jaw_pt0,...curve1[18])*arg.jaw_size;
  let jaw_ang0 = Math.atan2(curve1[18][1]-jaw_pt0[1],curve1[18][0]-jaw_pt0[0]);
  let jaw_ang =jaw_ang0- (arg.has_teeth*0.5+0.5)*arg.jaw_open*PI/4;
  let jaw_pt1 = [
    jaw_pt0[0]+Math.cos(jaw_ang)*jaw_l,
    jaw_pt0[1]+Math.sin(jaw_ang)*jaw_l,
  ]

  let [eye0,ef] = (arg.eye_type?fish_eye_b:fish_eye_a)(ex,ey,arg.eye_size);
  ef = clip_multi(ef,outline).true;

  let inlines = clip(inline,eye0).false;

  let lip0 = fish_lip(...jaw_pt0,...curve1[18],3);

  let lip1 = fish_lip(...jaw_pt0,...jaw_pt1,3);

  let [jc,jaw] = fish_jaw(...curve1[15-arg.mouth_size],...jaw_pt0,...jaw_pt1);

  jaw = clip_multi(jaw,lip1).false;
  jaw = clip_multi(jaw,outline).false;

  let teeth0s = [];
  let teeth1s = [];
  if (arg.has_teeth){
    let teeth0 = fish_teeth(...jaw_pt0,...curve1[18],arg.teeth_length,-1,arg.teeth_space);
    let teeth1 = fish_teeth(...jaw_pt0,...jaw_pt1,    arg.teeth_length,1,arg.teeth_space);

    teeth0s = clip_multi(teeth0,lip0).false;
    teeth1s = clip_multi(teeth1,lip1).false;
  }

  let olines = clip(outline,lip0).false;

  let lip0s = clip(lip0,lip1).false;

  let sh = shade_shape(outline,6,-6,-6);
  sh = clip_multi(sh,lip0).false;
  sh = clip_multi(sh,eye0).false;

  let sh2 = vein_shape(outline,arg.head_texture_amount);

  // let sh2 = patternshade_shape(outline,3,(x,y)=>{
  //   return noise(x*0.1,y*0.1)>0.6;
  // })

  sh2 = clip_multi(sh2,lip0).false;
  sh2 = clip_multi(sh2,eye0).false;
  
  let bbs = [];

  lip1s = [lip1];

  if (arg.has_moustache){
    let bb0 = barbel(...jaw_pt0,arg.moustache_length,PI*3/4,1.5);
    lip1s = clip(lip1,bb0).false;
    jaw = clip_multi(jaw,bb0).false;
    bbs.push(bb0);
  }

  if (arg.has_beard){
    let jaw_pt;
    if (jaw[0] && jaw[0].length){
      jaw_pt = jaw[0][~~(jaw[0].length/2)];
    }else{
      jaw_pt = curve1[8];
    }
    let bb1 = trsl_poly(barbel(...jaw_pt,arg.beard_length,PI*0.6+rand()*0.4-0.2),rand()*1-0.5,rand()*1-0.5);
    let bb2 = trsl_poly(barbel(...jaw_pt,arg.beard_length,PI*0.6+rand()*0.4-0.2),rand()*1-0.5,rand()*1-0.5);
    let bb3 = trsl_poly(barbel(...jaw_pt,arg.beard_length,PI*0.6+rand()*0.4-0.2),rand()*1-0.5,rand()*1-0.5);
  
    let bb3c = clip_multi([bb3],bb2).false;
    bb3c = clip_multi(bb3c,bb1).false;
    let bb2c = clip_multi([bb2],bb1).false;
    bbs.push(bb1,...bb2c,...bb3c);
  }

  let outlinel = [[0,0],[curve0[curve0.length-1][0],0],curve0[curve0.length-1],...curve2,curve1[0],[curve1[0][0],300],[0,300]];

  
  return [outlinel,[
    ...olines,...inlines,
    ...lip0s,...lip1s,
    ...ef,...sh,...sh2,
    ...bbs,
    ...teeth0s,...teeth1s,
    ...jaw]];
}

function bean(x){
  return Math.pow(0.25-Math.pow(x-0.5,2),0.5)*(2.6+2.4*Math.pow(x,1.5))*0.542;
}

function deviate(n){
  return rand()*2*n-n;
}


function fish(arg){
  let n = 32;
  let curve0 = [];
  let curve1 = [];
  if (arg.body_curve_type == 0){
    let s = arg.body_curve_amount;
    for (let i = 0; i < n; i++){
      let t = i/(n-1);
      
      let x =  225 + (t-0.5)*arg.body_length;
      let y = 150 - (Math.sin(t*PI)*lerp(0.5,1,noise(t*2,1))*s+(1-s))*arg.body_height;
      curve0.push([x,y]);
    }
    for (let i = 0; i < n; i++){
      let t = i/(n-1);
      let x =  225 + (t-0.5)*arg.body_length;
      let y = 150 + (Math.sin(t*PI)*lerp(0.5,1,noise(t*2,2))*s+(1-s))*arg.body_height;
      curve1.push([x,y]);
    }
  }else if (arg.body_curve_type == 1){
    for (let i = 0; i < n; i++){
      let t = i/(n-1);
      
      let x = 225 + (t-0.5)*arg.body_length;
      let y = 150-lerp(1-arg.body_curve_amount,1,lerp(0,1,noise(t*1.2,1))*bean(1-t))*arg.body_height;
      curve0.push([x,y]);
    }
    for (let i = 0; i < n; i++){
      let t = i/(n-1);
      let x = 225 + (t-0.5)*arg.body_length;
      let y = 150+lerp(1-arg.body_curve_amount,1,lerp(0,1,noise(t*1.2,2))*bean(1-t))*arg.body_height;
      curve1.push([x,y]);
    }
  }
  let outline = curve0.concat(curve1.slice().reverse());
  let sh = shade_shape(outline,8,-12,-12);

  let pattern_func;
  if (arg.pattern_type == 0){
    //none
    pattern_func = null;
  }else if (arg.pattern_type == 1){
    // pattern_func = (x,y)=>{
    //   return noise(x*0.1,y*0.1)>0.55;
    // };
    pattern_func = pattern_dot(arg.pattern_scale);
  }else if (arg.pattern_type == 2){
    pattern_func = (x,y)=>{
      return (noise(x*0.1,y*0.1) * Math.max(0.35,(y-10)/280) ) < 0.2 ;
    };
  }else if (arg.pattern_type == 3){
    pattern_func = (x,y)=>{
      let dx = noise(x*0.01,y*0.01)*30;
      return (~~((x+dx)/(30*arg.pattern_scale)))%2 == 1;
    };
  }else if (arg.pattern_type == 4){
    //small dot;
    pattern_func = null;
  }

  let bd;
  if (arg.scale_type == 0){
    bd = fish_body_a(curve0,curve1,arg.scale_scale,pattern_func);
  }else if (arg.scale_type == 1){
    bd = fish_body_b(curve0,curve1,arg.scale_scale,pattern_func);
  }else if (arg.scale_type == 2){
    bd = fish_body_c(curve0,curve1,arg.scale_scale);
  }else if (arg.scale_type == 3){
    bd = fish_body_d(curve0,curve1,arg.scale_scale);
  }

  let f0_func, f0_a0, f0_a1, f0_cv;
  if (arg.dorsal_type == 0){
    f0_a0 = 0.2 + deviate(0.05);
    f0_a1 = 0.3 + deviate(0.05);
    f0_cv = 0;
    f0_func = t=>(  (0.3+noise(t*3)*0.7)*arg.dorsal_length *Math.sin(t*PI)**0.5  );
  }else if (arg.dorsal_type == 1){
    f0_a0 = 0.6 + deviate(0.05);
    f0_a1 = 0.3 + deviate(0.05);
    f0_cv = arg.dorsal_length/8;
    f0_func = t=>(  arg.dorsal_length* ((Math.pow(t-1,2))*0.5 + (1-t)*0.5)  );
  }
  let f0_curve,c0,f0;
  if (arg.dorsal_texture_type == 0){
    f0_curve = resample(curve0.slice(arg.dorsal_start,arg.dorsal_end),5);
    ;[c0,f0] = fin_a(f0_curve,f0_a0,f0_a1,f0_func,false,f0_cv,0);
  }else{
    f0_curve = resample(curve0.slice(arg.dorsal_start,arg.dorsal_end),15);
    ;[c0,f0] = fin_b(f0_curve,f0_a0,f0_a1,f0_func);
  }
  f0 = clip_multi(f0,trsl_poly(outline,0,0.001)).false;
  
  let f1_curve = [];
  let f1_func, f1_a0, f1_a1, f1_soft, f1_cv;
  let f1_pt = lerp2d(...curve0[arg.wing_start],...curve1[arg.wing_end],arg.wing_y);

  for (let i = 0; i < 10; i++){
    let t = i/9;
    let y = lerp(f1_pt[1]-arg.wing_width/2,f1_pt[1]+arg.wing_width/2,t);
    f1_curve.push([f1_pt[0] /*+ Math.sin(t*PI)*2*/,y]);
  }
  if (arg.wing_type == 0){
    f1_a0 = -0.4 + deviate(0.05);
    f1_a1 = 0.4 + deviate(0.05);
    f1_soft = 10;
    f1_cv = 0;
    f1_func = t=>(  (40+(20+noise(t*3)*70)*Math.sin(t*PI)**0.5)/130*arg.wing_length  );
  }else{
    f1_a0 = 0 + deviate(0.05);
    f1_a1 = 0.4 + deviate(0.05);
    f1_soft = 5;
    f1_cv = arg.wing_length/25;
    f1_func = t=>(  arg.wing_length*(1-t*0.95)  ); 
  }
  
  let c1,f1;
  if (arg.wing_texture_type == 0){
    f1_curve = resample(f1_curve,1.5);
    ;[c1,f1] = fin_a(f1_curve,f1_a0,f1_a1,f1_func,1,f1_cv,0,f1_soft);
  }else{
    f1_curve = resample(f1_curve,4);
    ;[c1,f1] = fin_b(f1_curve,f1_a0,f1_a1,f1_func,0.3);
  }
  bd = clip_multi(bd,c1).false;


  let f2_curve;
  let f2_func, f2_a0, f2_a1;
  if (arg.pelvic_type == 0){
    f2_a0 = -0.8 + deviate(0.05);;
    f2_a1 = -0.5 + deviate(0.05);;
    f2_func = t=>(  (10+(15+noise(t*3)*60)*Math.sin(t*PI)**0.5)/85*arg.pelvic_length  );
  }else{
    f2_a0 = -0.9 + deviate(0.05);;
    f2_a1 = -0.3 + deviate(0.05);;
    f2_func = t=>( (t*0.5+0.5)*arg.pelvic_length  );
  }
  let c2,f2;
  if (arg.pelvic_texture_type == 0){
    f2_curve = resample(curve1.slice(arg.pelvic_start,arg.pelvic_end).reverse(),arg.pelvic_type?2:5);
    ;[c2,f2] = fin_a(f2_curve,f2_a0,f2_a1,f2_func);
  }else{
    f2_curve = resample(curve1.slice(arg.pelvic_start,arg.pelvic_end).reverse(),arg.pelvic_type?2:15);
    ;[c2,f2] = fin_b(f2_curve,f2_a0,f2_a1,f2_func);
  }
  f2 = clip_multi(f2,c1).false;

  let f3_curve;
  let f3_func, f3_a0, f3_a1;
  if (arg.anal_type == 0){
    f3_a0 = -0.4 + deviate(0.05);;
    f3_a1 = -0.4 + deviate(0.05);;
    f3_func = t=>(  (10+(10+noise(t*3)*30)*Math.sin(t*PI)**0.5)/50*arg.anal_length  );
  }else{
    f3_a0 = -0.4 + deviate(0.05);;
    f3_a1 = -0.4 + deviate(0.05);;
    f3_func = t=>(  arg.anal_length* (t*t*0.8+0.2)  );
  }
  let c3,f3;
  if (arg.anal_texture_type == 0){
    f3_curve = resample(curve1.slice(arg.anal_start,arg.anal_end).reverse(),5);
    ;[c3,f3] = fin_a(f3_curve,f3_a0,f3_a1,f3_func);
  }else{
    f3_curve = resample(curve1.slice(arg.anal_start,arg.anal_end).reverse(),15);
    ;[c3,f3] = fin_b(f3_curve,f3_a0,f3_a1,f3_func);
  }
  f3 = clip_multi(f3,c1).false;

  let f4_curve, c4, f4;
  let f4_r = dist(...curve0[curve0.length-2],...curve1[curve1.length-2]);
  let f4_n = ~~(f4_r/1.5);
  f4_n = Math.max(Math.min(f4_n,20),8);
  let f4_d = f4_r/f4_n;
  // console.log(f4_n,f4_d);
  if (arg.tail_type == 0){
    f4_curve = [curve0[curve0.length-1], curve1[curve1.length-1]];
    f4_curve = resample(f4_curve,f4_d);
    ;[c4,f4] = fin_a(f4_curve,-0.6,0.6,t=>(  (75-(10+noise(t*3)*10)*Math.sin(3*t*PI-PI))/75*arg.tail_length  ),1);  
  }else if (arg.tail_type == 1){
    f4_curve = [curve0[curve0.length-2], curve1[curve1.length-2]];
    f4_curve = resample(f4_curve,f4_d);
    ;[c4,f4] = fin_a(f4_curve,-0.6,0.6,t=>( arg.tail_length*(Math.sin(t*PI)*0.5+0.5)  ),1); 
  }else if (arg.tail_type == 2){
    f4_curve = [curve0[curve0.length-1], curve1[curve1.length-1]];
    f4_curve = resample(f4_curve,f4_d*0.7);
    let cv = arg.tail_length/8;
    ;[c4,f4] = fin_a(f4_curve,-0.6,0.6,t=>(  (Math.abs(Math.cos(PI*t))*0.8+0.2)*arg.tail_length  ),1,cv,-cv);  
  }else if (arg.tail_type == 3){
    f4_curve = [curve0[curve0.length-2], curve1[curve1.length-2]];
    f4_curve = resample(f4_curve,f4_d);
    ;[c4,f4] = fin_a(f4_curve,-0.6,0.6,t=>(  (1-Math.sin(t*PI)*0.3)*arg.tail_length  ),1);  
  }else if (arg.tail_type == 4){
    f4_curve = [curve0[curve0.length-2], curve1[curve1.length-2]];
    f4_curve = resample(f4_curve,f4_d);
    ;[c4,f4] = fin_a(f4_curve,-0.6,0.6,t=>(  (1-Math.sin(t*PI)*0.6)*(1-t*0.45)*arg.tail_length  ),1);  
  }else if (arg.tail_type == 5){
    f4_curve = [curve0[curve0.length-2], curve1[curve1.length-2]];
    f4_curve = resample(f4_curve,f4_d);
    ;[c4,f4] = fin_a(f4_curve,-0.6,0.6,t=>(  (1-Math.sin(t*PI)**0.4*0.55)*arg.tail_length  ),1);
  }
  // f4 = clip_multi(f4,trsl_poly(outline,-1,0)).false;
  bd = clip_multi(bd,trsl_poly(c4,1,0)).false;

  f4 = clip_multi(f4,c1).false;

  let f5_curve,c5,f5=[];
  if (arg.finlet_type == 0){
    //pass
  }else if (arg.finlet_type == 1){
    f5_curve = resample(curve0.slice(arg.dorsal_end,-2),5);
    ;[c5,f5] = finlet(f5_curve,5);
    f5_curve = resample(curve1.slice(arg.anal_end,-2).reverse(),5);
    if (f5_curve.length>1){
      ;[c5,f5] = finlet(f5_curve,5);
    }
  }else if (arg.finlet_type == 2){
    f5_curve = resample(curve0.slice(27,30),5);
    ;[c5,f5] = fin_adipose(f5_curve,20,-5,6);
    outline = poly_union(outline,trsl_poly(c5,0,-1));
  }else{
    f5_curve = resample(curve0.slice(arg.dorsal_end+2,-3),5);
    if (f5_curve.length>2){
      ;[c5,f5] = fin_a(f5_curve,0.2,0.3, t=>(  (0.3+noise(t*3)*0.7)*arg.dorsal_length*0.6 *Math.sin(t*PI)**0.5  ));
    }
    
  }
  let cf,fh;
  if (arg.neck_type == 0){
    ;[cf,fh] = fish_head(50-arg.head_length,150+arg.nose_height,...curve0[6],...curve1[5],arg);
  }else{
    ;[cf,fh] = fish_head(50-arg.head_length,150+arg.nose_height,...curve0[5],...curve1[6],arg);
  }
  bd = clip_multi(bd,cf).false;
  
  sh = clip_multi(sh,cf).false;
  sh = clip_multi(sh,c1).false;

  f1 = clip_multi(f1,cf).false;

  f0 = clip_multi(f0,c1).false;

  let sh2 = [];
  if (pattern_func){
    if (arg.scale_type > 1){
      sh2 = patternshade_shape( poly_union(outline,trsl_poly(c0,0,3)  ),3.5,pattern_func);
    }else{
      sh2 = patternshade_shape( c0,4.5,pattern_func);
    }
    sh2 = clip_multi(sh2,cf).false;
    sh2 = clip_multi(sh2,c1).false;
  }

  let sh3 = [];
  if (arg.pattern_type == 4){
    sh3 = smalldot_shape( poly_union(outline,trsl_poly(c0,0,5)  ), arg.pattern_scale);
    sh3 = clip_multi(sh3,c1).false;
    sh3 = clip_multi(sh3,cf).false;
  }

  return bd.
  concat(f0).
  concat(f1).
  concat(f2).
  concat(f3).
  concat(f4).
  concat(f5).
  concat(fh).
  concat(sh).
  concat(sh2).
  concat(sh3).
  // concat([cf]).
  concat();
}

function reframe(polylines,pad=20,text=null){
  
  let W = (500-pad*2);
  let H = (300-pad*2) - (text?10:0);
  let bbox = get_bbox(polylines.flat());
  let sw = W/bbox.w;
  let sh = H/bbox.h;
  let s = Math.min(sw,sh);
  let px = (W-bbox.w*s)/2;
  let py = (H-bbox.h*s)/2;
  for (let i = 0; i < polylines.length; i++){
    for (let j = 0; j < polylines[i].length; j++){
      let [x,y] = polylines[i][j];
      x = (x - bbox.x) * s + px+pad;
      y = (y - bbox.y) * s + py+pad;
      polylines[i][j] = [x,y];
    }
  }
  let [tw,tp] = put_text(text);
  tp = tp.map(p=>scl_poly(shr_poly(p,-0.3),0.3,0.3));
  tw *= 0.3;
  polylines.push(...tp.map(p=>trsl_poly(p,250-tw/2,300-pad+5)));
  return polylines;
}

function cleanup(polylines){
  for (let i = polylines.length-1; i>=0; i--){
    polylines[i] = approx_poly_dp(polylines[i],0.1);
    for (let j = 0; j < polylines[i].length; j++){
      for (let k = 0; k < polylines[i][j].length; k++){
        polylines[i][j][k] = ~~(polylines[i][j][k]*10000)/10000;
      }
    }
    if (polylines[i].length < 2){
      polylines.splice(i,1);
      continue;
    }
    if (polylines[i].length == 2){
      if (dist(...polylines[0],...polylines[1])<0.9){
        polylines.splice(i,1);
        continue;
      }
    }
  }
  return polylines;
}

function default_params(){
  return {
    body_curve_type:0,
    body_curve_amount:0.85,
    body_length:350,
    body_height:90,
    scale_type:1,
    scale_scale:1,
    pattern_type:3,
    pattern_scale:1,
    dorsal_texture_type:1,
    dorsal_type:0,
    dorsal_length:100,
    dorsal_start:8,
    dorsal_end:27,
    wing_texture_type:0,
    wing_type:0,
    wing_start:6,
    wing_end:6,
    wing_y:0.7,
    wing_length:130,
    wing_width:10,
    pelvic_start:9,
    pelvic_end:14,
    pelvic_length:85,
    pelvic_type:0,
    pelvic_texture_type:0,
    anal_start:19,
    anal_end:29,
    anal_length:50,
    anal_type:0,
    anal_texture_type:0,
    tail_type:0,
    tail_length:75,
    finlet_type:0,
    neck_type:0,
    nose_height:0,
    mouth_size:8,
    head_length:30,
    head_texture_amount:60,
    has_moustache:1,
    moustache_length:10,
    has_beard:0,
    has_teeth:1,
    teeth_length:8,
    teeth_space:3.5,
    beard_length:30,
    eye_type:1,
    eye_size:10,
    jaw_size:1,
    jaw_open:1,
  }
}

function choice(opts,percs){
  if (!percs){
    percs = opts.map(x=>1);
  }
  let s = 0;
  for (let i = 0; i < percs.length; i++){
    s += percs[i];
  }
  let r = rand()*s;
  s = 0;
  for (let i = 0; i < percs.length; i++){
    s += percs[i];
    if (r <= s){
      return opts[i];
    }
  }
}

function rndtri(a,b,c){
  let s0 = (b-a)/2;
  let s1 = (c-b)/2;
  let s = s0 + s1;
  let r = rand()*s;
  if (r < s0){
    //d * d/(b-a) / 2 = r;
    let d = Math.sqrt(2*r*(b-a));
    return a + d;
  }
  //d * d/(c-b) / 2 = s-r;
  let d = Math.sqrt(2*(s-r)*(c-b));
  return c-d;
}

function generate_params(){

  let arg = default_params();
  arg.body_curve_type =   choice([0,1]);
  arg.body_curve_amount = rndtri(0.5,0.85,0.98);
  arg.body_length = rndtri(200,350,420);
  arg.body_height = rndtri(45,90,150);
  arg.scale_type = choice([0,1,2,3]);
  arg.scale_scale = rndtri(0.8,1,1.5);
  arg.pattern_type = choice([0,1,2,3,4]);
  arg.pattern_scale = rndtri(0.5,1,2);
  arg.dorsal_texture_type = choice([0,1]);
  arg.dorsal_type = choice([0,1]);
  arg.dorsal_length = rndtri(30,90,180);
  if (arg.dorsal_type == 0){
    arg.dorsal_start = ~~rndtri(7,8,15);
    arg.dorsal_end   = ~~rndtri(20,27,28);
  }else{
    arg.dorsal_start = ~~rndtri(11,12,16);
    arg.dorsal_end   = ~~rndtri(19,21,24);
  }
  arg.wing_texture_type = choice([0,1]);
  arg.wing_type = choice([0,1]);
  if (arg.wing_type == 0){
    arg.wing_length = rndtri(40,130,200);
  }else{
    arg.wing_length = rndtri(40,150,350);
  }
  if (arg.wing_texture_type == 0){
    arg.wing_width = rndtri(7,10,20);
    arg.wing_y = rndtri(0.45,0.7,0.85);
  }else{
    arg.wing_width = rndtri(20,30,50);
    arg.wing_y = rndtri(0.45,0.65,0.75);
  }
  
  arg.wing_start = ~~rndtri(5,6,8);
  arg.wing_end = ~~rndtri(5,6,8);

  arg.pelvic_texture_type = arg.dorsal_texture_type ? choice([0,1]) : 0;
  arg.pelvic_type = choice([0,1]);
  arg.pelvic_length = rndtri(30,85,140);
  if (arg.pelvic_type == 0){
    arg.pelvic_start = ~~rndtri(7,9,11);
    arg.pelvic_end = ~~rndtri(13,14,15);
  }else{
    arg.pelvic_start = ~~rndtri(7,9,12);
    arg.pelvic_end = arg.pelvic_start+2;
  }

  arg.anal_texture_type = arg.dorsal_texture_type ? choice([0,1]) : 0;
  arg.anal_type = choice([0,1]);
  arg.anal_length = rndtri(20,50,80);
  arg.anal_start = ~~rndtri(16,19,23);
  arg.anal_end = ~~rndtri(25,29,31);

  arg.tail_type = choice([0,1,2,3,4,5]);
  arg.tail_length = rndtri(50,75,180);

  arg.finlet_type = choice([0,1,2,3]);

  arg.neck_type = choice([0,1]);
  arg.nose_height = rndtri(-50,0,35);
  arg.head_length = rndtri(20,30,35);
  arg.mouth_size = ~~rndtri(6,8,11);

  arg.head_texture_amount = ~~rndtri(30,60,160);
  arg.has_moustache = choice([0,0,0,1]);
  arg.has_beard = choice([0,0,0,0,0,1]);
  arg.moustache_length = ~~rndtri(10,20,40);
  arg.beard_length = ~~rndtri(20,30,50);

  arg.eye_type = choice([0,1]);
  arg.eye_size = rndtri(8,10,28)//arg.body_height/6//Math.min(arg.body_height/6,rndtri(8,10,30));

  arg.jaw_size = rndtri(0.7,1,1.4);

  arg.has_teeth = choice([0,1,1]);
  arg.teeth_length = rndtri(5,8,15);
  arg.teeth_space = rndtri(3,3.5,6);

  return arg;
}


function binomen(){
  let data =[["A","AB","AL","AN","AP","AR","AU","BA","BE","BO","BRA","CA","CAR","CENT","CHAE","CHAN","CHI","CHRO","CHRY","CO","CTE","CY","CYP","DE","E","EU","GA","GAS","GNA","GO","HE","HIP","HO","HY","LA","LAB","LE","LI","LO","LU","MAC","ME","MIC","MO","MU","MY","NA","NAN","NE","NO","O","ON","OP","OS","PA","PER","PHO","PI","PLA","PLEU","PO","PSEU","PTE","RA","RHI","RHOM","RU","SAL","SAR","SCA","SCOM","SE","SI","STE","TAU","TEL","THO","TRI","XE","XI"],
  ["BE","BI","BO","BU","CA","CAM","CAN","CE","CENT","CHA","CHEI","CHI","CHO","CHY","CI","CIRR","CO","DI","DO","DON","DOP","GA","GAS","GO","HI","HYN","LA","LAB","LE","LEOT","LI","LICH","LIS","LO","LOS","LU","LY","MA","ME","MI","MICH","MO","MU","NA","NE","NEC","NI","NO","NOCH","NOP","NOS","PA","PE","PEN","PHA","PHI","PHO","PHY","PHYO","PI","PIP","PIS","PO","POG","POPH","RA","RAE","RAM","REOCH","RI","RICH","RIP","RIS","RO","ROI","ROP","ROS","RY","RYN","SE","SO","TA","TE","TEL","THAL","THE","THO","THOP","THU","TI","TICH","TO","TOG","TOP","TOS","VA","XI","XO"],
  ["BIUS","BUS","CA","CHUS","CION","CON","CUS","DA","DES","DEUS","DON","DUS","GER","GON","GUS","HUS","LA","LEA","LIS","LIUS","LUS","MA","MIS","MUS","NA","NIA","NIO","NIUS","NOPS","NUS","PHEUS","PHIS","PIS","PUS","RA","RAS","RAX","RIA","RION","RIS","RUS","RYS","SA","SER","SIA","SIS","SUS","TER","TES","TEUS","THUS","THYS","TIA","TIS","TUS","TYS"],
  ["A","AE","AL","AN","AR","AT","AU","AUST","AY","BA","BAR","BE","BI","BO","CA","CAL","CAM","CAN","CAR","CAU","CE","CHI","CHRY","COR","CRY","CU","CYA","DA","DE","DEN","DI","DIA","DO","DOR","DU","E","FA","FAS","FES","FI","FLO","FOR","FRE","FUR","GLA","GO","HA","HE","HIP","HO","HYP","I","IM","IN","JA","LA","LAB","LE","LEU","LI","LO","LU","MA","MAC","MAR","ME","MO","MOO","MOR","NA","NE","NI","NIG","NO","O","OR","PA","PAL","PE","PEC","PHO","PLA","PLU","PO","PRO","PU","PUL","RA","RE","RHOM","RI","RO","ROST","RU","SA","SAL","SE","SO","SPI","SPLEN","STRIA","TAU","THO","TRI","TY","U","UN","VA","VI","VIT","VUL","WAL","XAN"],
  ["BA","BAR","BER","BI","BO","BOI","BU","CA","CAN","CAU","CE","CEL","CHA","CHEL","CHOP","CI","CIA","CIL","CIO","CO","COS","CU","DA","DE","DEL","DI","DIA","DO","FAS","FEL","FI","FOR","GA","GE","GI","HA","HYN","KE","LA","LAN","LE","LEA","LEU","LI","LIA","LO","LON","LOP","MA","ME","MEN","MI","MIE","MO","NA","NE","NEA","NEL","NEN","NI","NIF","NO","NOI","NOP","NU","PA","PE","PER","PHA","PHE","PI","PIN","PO","QUI","RA","RAC","RE","REN","RES","RI","RIA","RIEN","RIF","RO","ROR","ROS","ROST","RU","RYTH","SA","SE","SI","SO","SU","TA","TAE","TE","TER","THAL","THO","THU","TI","TIG","TO","TU","VA","VE","VES","VI","VIT","XEL","XI","ZO"],
  ["BEUS","CA","CENS","CEPS","CEUS","CHA","CHUS","CI","CUS","DA","DAX","DENS","DES","DI","DIS","DUS","FER","GA","GI","GUS","KEI","KI","LA","LAS","LI","LIS","LIUS","LOR","LUM","LUS","MA","MIS","MUS","NA","NEUS","NI","NII","NIS","NIUS","NUS","PIS","PUS","RA","RE","RI","RIAE","RIE","RII","RIO","RIS","RIX","RONS","RU","RUM","RUS","SA","SEUS","SI","SIS","SUS","TA","TEUS","THUS","TI","TIS","TOR","TUM","TUS","TZI","ZI"]]
  let freq =[[27,2,4,4,2,2,2,5,2,2,3,4,2,5,3,2,2,2,3,8,3,3,2,2,7,2,3,2,2,2,6,3,2,4,5,2,5,2,3,2,2,5,5,4,2,3,2,3,2,2,9,2,2,2,7,2,2,2,2,2,5,6,2,2,2,2,2,2,2,2,2,4,2,2,2,2,3,3,2,3],
  [2,2,3,3,5,2,11,6,4,2,7,2,4,4,3,3,5,4,9,2,2,4,5,13,3,3,12,3,3,2,8,3,4,15,6,2,3,10,3,3,2,2,2,8,7,3,4,20,2,2,3,4,3,2,10,2,6,2,2,5,2,2,13,2,2,14,3,2,2,9,4,2,5,42,2,4,2,6,3,3,11,2,19,2,3,2,5,3,2,4,2,27,2,2,2,2,2,2],
  [3,3,7,7,3,2,3,2,5,2,13,7,2,3,4,2,13,2,2,2,24,18,13,17,12,4,2,5,3,19,3,2,2,3,7,3,2,5,2,6,29,3,2,2,2,3,4,4,16,2,6,12,5,5,6,2],
  [23,3,11,6,6,3,8,2,2,2,3,3,9,3,8,2,2,3,2,2,2,2,6,2,2,3,4,3,4,2,2,2,2,2,2,15,2,4,2,2,2,2,2,2,2,2,2,5,3,2,2,3,2,2,2,7,2,2,3,3,4,4,13,7,3,10,2,2,2,5,2,3,6,4,14,2,3,2,5,2,2,3,2,3,2,2,2,3,5,2,2,3,2,3,5,2,5,2,3,3,3,3,3,7,2,3,2,4,3,2,2,2,2],
  [5,2,2,4,4,2,2,10,6,2,3,5,3,2,2,6,12,2,3,6,2,22,4,4,2,7,5,6,10,2,2,2,9,7,4,2,2,2,39,3,10,3,2,20,2,10,2,2,12,9,3,8,2,4,19,5,5,3,3,12,2,9,3,2,7,3,4,3,6,2,8,5,2,4,25,2,4,3,2,26,2,2,2,21,2,2,4,6,5,3,6,4,6,2,14,2,19,2,2,2,2,21,3,14,2,3,5,2,5,2,2,2,3],
  [2,7,4,3,5,2,5,2,13,6,2,2,6,8,2,4,3,4,2,5,2,5,11,3,7,19,2,2,2,11,10,4,6,12,3,15,4,6,2,18,3,3,11,4,14,2,2,3,2,13,2,3,2,4,21,7,2,10,8,13,31,2,5,5,2,2,10,68,2,3]]
    
  let name = choice(data[0],freq[0]);
  let n = ~~(rand()*3);
  for (let i = 0; i < n; i++){
    name += choice(data[1],freq[1]);
  }
  name += choice(data[2],freq[2]);
  name += ' ';
  name += choice(data[3],freq[3]);
  n = ~~(rand()*3);
  for (let i = 0; i < n; i++){
    name += choice(data[4],freq[4]);
  }
  name += choice(data[5],freq[5]);
  name = name.replace(/([A-Z])\1\1+/g,'$1$1');
  return name[0]+name.slice(1).toLowerCase();
}

let hershey_raw = {
"501":"  9I[RFJ[ RRFZ[ RMTWT",
"502":" 24G\\KFK[ RKFTFWGXHYJYLXNWOTP RKPTPWQXRYTYWXYWZT[K[",
"503":" 19H]ZKYIWGUFQFOGMILKKNKSLVMXOZQ[U[WZYXZV",
"504":" 16G\\KFK[ RKFRFUGWIXKYNYSXVWXUZR[K[",
"505":" 12H[LFL[ RLFYF RLPTP RL[Y[",
"506":"  9HZLFL[ RLFYF RLPTP",
"507":" 23H]ZKYIWGUFQFOGMILKKNKSLVMXOZQ[U[WZYXZVZS RUSZS",
"508":"  9G]KFK[ RYFY[ RKPYP",
"509":"  3NVRFR[",
"510":" 11JZVFVVUYTZR[P[NZMYLVLT",
"511":"  9G\\KFK[ RYFKT RPOY[",
"512":"  6HYLFL[ RL[X[",
"513":" 12F^JFJ[ RJFR[ RZFR[ RZFZ[",
"514":"  9G]KFK[ RKFY[ RYFY[",
"515":" 22G]PFNGLIKKJNJSKVLXNZP[T[VZXXYVZSZNYKXIVGTFPF",
"516":" 14G\\KFK[ RKFTFWGXHYJYMXOWPTQKQ",
"517":" 25G]PFNGLIKKJNJSKVLXNZP[T[VZXXYVZSZNYKXIVGTFPF RSWY]",
"518":" 17G\\KFK[ RKFTFWGXHYJYLXNWOTPKP RRPY[",
"519":" 21H\\YIWGTFPFMGKIKKLMMNOOUQWRXSYUYXWZT[P[MZKX",
"520":"  6JZRFR[ RKFYF",
"521":" 11G]KFKULXNZQ[S[VZXXYUYF",
"522":"  6I[JFR[ RZFR[",
"523":" 12F^HFM[ RRFM[ RRFW[ R\\FW[",
"524":"  6H\\KFY[ RYFK[",
"525":"  7I[JFRPR[ RZFRP",
"526":"  9H\\YFK[ RKFYF RK[Y[",
"601":" 18I\\XMX[ RXPVNTMQMONMPLSLUMXOZQ[T[VZXX",
"602":" 18H[LFL[ RLPNNPMSMUNWPXSXUWXUZS[P[NZLX",
"603":" 15I[XPVNTMQMONMPLSLUMXOZQ[T[VZXX",
"604":" 18I\\XFX[ RXPVNTMQMONMPLSLUMXOZQ[T[VZXX",
"605":" 18I[LSXSXQWOVNTMQMONMPLSLUMXOZQ[T[VZXX",
"606":"  9MYWFUFSGRJR[ ROMVM",
"607":" 23I\\XMX]W`VaTbQbOa RXPVNTMQMONMPLSLUMXOZQ[T[VZXX",
"608":" 11I\\MFM[ RMQPNRMUMWNXQX[",
"609":"  9NVQFRGSFREQF RRMR[",
"610":" 12MWRFSGTFSERF RSMS^RaPbNb",
"611":"  9IZMFM[ RWMMW RQSX[",
"612":"  3NVRFR[",
"613":" 19CaGMG[ RGQJNLMOMQNRQR[ RRQUNWMZM\\N]Q][",
"614":" 11I\\MMM[ RMQPNRMUMWNXQX[",
"615":" 18I\\QMONMPLSLUMXOZQ[T[VZXXYUYSXPVNTMQM",
"616":" 18H[LMLb RLPNNPMSMUNWPXSXUWXUZS[P[NZLX",
"617":" 18I\\XMXb RXPVNTMQMONMPLSLUMXOZQ[T[VZXX",
"618":"  9KXOMO[ ROSPPRNTMWM",
"619":" 18J[XPWNTMQMNNMPNRPSUTWUXWXXWZT[Q[NZMX",
"620":"  9MYRFRWSZU[W[ ROMVM",
"621":" 11I\\MMMWNZP[S[UZXW RXMX[",
"622":"  6JZLMR[ RXMR[",
"623":" 12G]JMN[ RRMN[ RRMV[ RZMV[",
"624":"  6J[MMX[ RXMM[",
"625":" 10JZLMR[ RXMR[P_NaLbKb",
"626":"  9J[XMM[ RMMXM RM[X[",
"710":"  6MWRYQZR[SZRY",
};

let hershey_cache = {};

function compile_hershey(i){
  if (hershey_cache[i]){
    return hershey_cache[i];
  }
  var entry = hershey_raw[i];
  if (entry == null){
    return;
  }
  var ordR = 82;
  var bound= entry.substring(3,5);
  var xmin = bound.charCodeAt(0)-ordR;
  var xmax = bound.charCodeAt(1)-ordR;
  var content = entry.substring(5);
  var polylines = [[]];
  var j  = 0;
  while (j < content.length){
    var digit = content.substring(j,j+2);
    if (digit == " R"){
      polylines.push([]);
    }else{
      var x  = digit.charCodeAt(0)-ordR;
      var y  = digit.charCodeAt(1)-ordR;
      polylines[polylines.length-1].push([x,y]);
    }
    j+=2;
  }
  let data = {
    xmin:xmin,
    xmax:xmax,
    polylines:polylines,
  };
  hershey_cache[i] = data;
  return data;
}

function put_text(txt){
  let base = 500;
  let x = 0;
  let o = [];
  for (let i = 0; i < txt.length; i++){
    let ord = txt.charCodeAt(i);
    let idx;
    if (65 <= ord && ord <= 90){
      idx = base+1+(ord-65);
    }else if (97 <= ord && ord <= 122){
      idx = base + 101+(ord-97);
    }else if (ord == 46){
      idx = 710;
    }else if (ord == 32){
      x += 10;
      continue;
    }else{
      continue;
    }
    let {xmin,xmax,polylines} = compile_hershey(idx);
    polylines = polylines.map(p=>trsl_poly(p,x-xmin,0));
    o.push(...polylines);
    x += (xmax-xmin);
  }
  return [x,o];
}

function str_to_seed(str){
  let n = 1;
  for (let i = 0; i < str.length; i++){
    let x = str.charCodeAt(i)+1;
    n ^= x << (7+(i%5));
    // if (i % 2){
      n ^=(n<<17);
      n ^=(n>>13);
      n ^=(n<<5);
    // }
    n = (n>>>0) % 4294967295;
  }
  return n;
}

function main(seed){
  if (seed === undefined){
    jsr = ~~(Math.random()*10000);
    let name = binomen();
    seed = name;
  }
  jsr = str_to_seed(seed);
  let drawing = fish(generate_params());
  return (cleanup(reframe(drawing,20,seed+'.')));
}


if (typeof module != "undefined"){
  module.exports = {main,generate_params,default_params,fish,reframe,cleanup,draw_svg,binomen,str_to_seed};
  if (require.main === module) {
    let seed = undefined;
    let format = 'svg';
    let speed = 0.005;
    for (let i = 2; i < process.argv.length; i++){
      let a = process.argv[i];
      if (a == '--seed'){
        seed = process.argv[i+1];
      }else if (a == '--format'){
        format = process.argv[i+1];
      }else if (a == '--speed'){
        if (process.argv[i+1] > 0)
          speed = speed / process.argv[i+1];
      }
    }
    let polylines = main(seed);
    if (format == 'svg'){
      console.log(draw_svg(polylines));
    }else if (format == 'json'){
      console.log(JSON.stringify(polylines));
    }else if (format == 'smil'){
      console.log(draw_svg_anim(polylines,speed));
    }else if (format == 'csv'){
      console.log(polylines.map(x=>x.flat().join(',')).join('\n'));
    }else if (format == 'ps'){
      console.log(draw_ps(polylines));
    }
  }
}
