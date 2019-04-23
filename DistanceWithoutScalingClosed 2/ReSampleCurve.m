function Xn = ReSampleCurve(X,N)

    T = length(X);
    del(1)=0;
    for r = 2:T
        del(r) = norm(X(:,r) - X(:,r-1));
    end
    ind=find(del(2:T)==0)+1;
    if length(ind)>0
        del(ind)=[];
        X(:,ind)=[];
    end
    T = length(X);
    cumdel = cumsum(del)/sum(del);
    
    newdel = [0:N-1]/(N-1);
    
    Xn(1,:) = interp1(cumdel,X(1,1:T),newdel,'linear');
    Xn(2,:) = interp1(cumdel,X(2,1:T),newdel,'linear');