function [q2best,Rbest] = Find_Rotation_and_Seed_unique(q1,q2,reparamFlag)
% This function returns a locally optimal rotation and seed point for shape
% q2 w.r.t. q1

[n,T] = size(q1);

scl = 4;
minE = 1000;
for ctr = 0:floor((T)/scl)
    q2n = ShiftF(q2,scl*ctr);
    [q2n,R] = Find_Best_Rotation(q1,q2n);
    
    if(reparamFlag)
        
        if norm(q1-q2n,'fro') > 0.0001
            gamI = DynamicProgrammingQ(q2n/sqrt(InnerProd_Q(q2n,q2n)),q1/sqrt(InnerProd_Q(q1,q1)),0,0);
            gamI = (gamI-gamI(1))/(gamI(end)-gamI(1));
            p2n = q_to_curve(q2n);
            p2new = Group_Action_by_Gamma_Coord(p2n,gamI);
            q2new = curve_to_q(p2new);
            q2new = ProjectC(q2new);
        else
            q2new = q2n;
        end
        
    else
        q2new  = q2n;
    end
    Ec = sqrt(InnerProd_Q(q1-q2new,q1-q2new));
    
    if Ec < minE
        Rbest=R;
%         Rbest=[1,0;0,1];
        q2best  = q2new;
        minE = Ec;
    end
end
return;